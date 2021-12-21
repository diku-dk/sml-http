structure Http :> HTTP = struct

  infix |>
  fun x |> f = f x

  open ScanUtil
  infix >>> ->> >>- >>? || >>@ >>* ??

  fun scanAnyChar get s = get s

 (* memo: instead of building a list, we should be able to extract the string from the
    underlying slice *)

  fun scanAnyChars get =
      (list scanAnyChar >>@ implode) get

  val p_space : (unit,'st) p =
   fn g => ign (scanChar (fn c => c = #" ")) g

  fun skipChars f : (unit,'st) p =
      fn g => fn s =>
         case g s of
             SOME(c,s') =>
             if f c then skipChars f g s'
             else SOME((),s)
           | NONE => SOME((),s)

  structure Util = HttpUtil

  structure Uri = struct
    datatype t = URL of {scheme: string, host: string,
                         port: int option, path: string,
                         query: string}   (* http://domain:port/path?query *)
               | AST
               | PATH of {path: string,
                          query: string}  (* /path?query *)

    val p_scheme : (string, 'st) p =
     fn g => (str "https" || str "http") g

    val p_host : (string, 'st) p =
     fn g => scanChars (fn c => c = #"." orelse
                                c = #"-" orelse
                                Char.isAlphaNum c) g
    val p_port : (int, 'st) p =
     fn g => (scanChars Char.isDigit ?? Int.fromString) g

    val p_path : (string, 'st) p =
     fn g => (scanChars (fn c => not(Char.isSpace c) andalso c <> #"?")) g

    val p_query : (string, 'st) p =
     fn g => (str "?" ->>
              scanChars (fn c => not(Char.isSpace c))) g

    val parse_url : (t, 'st) p =
     fn g =>
        ((p_scheme >>- str "://") >>>
         p_host >>>
         option (str ":" ->> p_port) >>>
         option (con ("/",())) >>>
         option p_path >>>
         option p_query ??
        (fn (((((s, h), port), sep), path), query) =>
            let val path = case (sep, path) of
                               (SOME(), SOME p) => SOME ("/" ^ p)
                             | (_, NONE) => SOME "/"
                             | (NONE, SOME _) => NONE
            in case path of
                   NONE => NONE
                 | SOME path =>
                   SOME (URL {scheme=s,host=h,port=port,path=path,
                              query=Option.getOpt(query,"")})
            end)) g

    val parse_ast : (t, 'st) p =
     fn g => con ("*",AST) g

    val parse_path : (t, 'st) p =
       fn g =>
          (con ("/",()) ->>
           option p_path >>>
           option p_query >>@
           (fn (path, query) =>
               let val path = case path of NONE => "/"
                                         | SOME p => "/" ^ p
               in PATH {path=path,query=Option.getOpt(query,"")}
               end)) g

    val parse : (t, 'st) p =
        fn g => (parse_url || parse_ast || parse_path) g

    fun toString t =
        case t of
            URL {scheme, host, port, path, query} =>
            scheme ^ "://" ^ host ^
            (case port of
                 SOME i => ":" ^ Int.toString i
               | NONE => "") ^
            path ^
            (case query of "" => "" | _ => "?" ^ query)
          | AST => "*"
          | PATH {path,query} =>
            path ^
            (case query of "" => "" | _ => "?" ^ query)
  end

  structure Mime = struct
    open HttpMime

    val parse : (t, 'st) p =
     fn g => (scanChars (fn c => Char.isAlphaNum c orelse
                                 CharVector.exists (fn k => c=k) "+-/.")
              ?? fromString) g
  end

  structure Version = struct
    type t = string  (* HTTP/1.0, HTTP/1.1, ... *)
    val parse : (t, 'st) p =
     fn g => (str "HTTP/1.0" || str "HTTP/1.1") g
    fun toString x = x
    val HTTP_1_0 = "HTTP/1.0"
    val HTTP_1_1 = "HTTP/1.1"
  end

  structure StatusCode = struct
    open HttpStatus
    val parse : (t, 'st) p =  (* 3 digits *)
     fn g => (scanChars Char.isDigit ?? fromString) g

    val OK = "200"
    val Redirect = "302"
    val BadRequest = "400"
    val Forbidden = "403"
    val NotFound = "404"
    val LengthRequired = "411"
    val InternalServerError = "500"
  end

  structure Header = struct
    type t = string * string

    fun removeTrailingWS s =
        let fun loop i =
                if i > 0 andalso Char.isSpace(String.sub(s,i-1))
                then loop (i-1)
                else if i = size s then s
                else if i <= 0 then ""
                else String.extract(s,0,SOME i)
        in loop (size s)
        end

    val parse : (string*string, 'st) p =
     fn g => (scanChars (fn c => Char.isPrint c andalso
                                 c <> #":") >>-
              str ":" >>>
              (skipWS(scanChars (fn c => Char.isPrint c andalso
                                         c <> #"\r" andalso
                                         c <> #"\n")) >>@
                     removeTrailingWS)
             ) g
    fun toString (n,v) =
        n ^ ":" ^ v

    fun keyEq x y =
        size x = size y andalso
        let fun loop i =
                if i < 0 then true
                else Char.toLower (String.sub(x,i)) =
                     Char.toLower (String.sub(y,i)) andalso loop (i-1)
        in loop (size x - 1)
        end

    fun look nil k = NONE
      | look ((x,y)::rest) k = if keyEq x k then SOME y
                               else look rest k

    fun lookAll pairs k =
        let fun loop nil acc = rev acc
              | loop ((x,y)::rest) acc =
                if keyEq x k then loop rest (k::acc)
                else loop rest acc
        in loop pairs nil
        end

  end

  structure Request = struct
    datatype method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

    type line = {method: method, uri: Uri.t, version: Version.t}
    type t = {line:line, headers: (string*string)list, body: string option}

    val parse_method  : (method, 'st) p =
     fn g => (con ("OPTIONS", OPTIONS) ||
              con ("GET", GET) ||
              con ("HEAD", HEAD) ||
              con ("POST", POST) ||
              con ("PUT", PUT) ||
              con ("DELETE", DELETE) ||
              con ("TRACE", TRACE) ||
              con ("CONNECT", CONNECT)) g

    val parse_headers : ((string*string)list, 'st) p =
     fn g => list (Header.parse >>- str "\r\n") g

    val parse_line : (line, 'st) p =
     fn g => (parse_method >>- p_space >>>
              Uri.parse >>- p_space >>>
              Version.parse >>@
              (fn ((m,u),v) => {method=m,uri=u,version=v})) g

    val parse_line_and_headers : (line*Header.t list, 'st) p =
        fn g => (parse_line >>-
                 str "\r\n" >>>
                 parse_headers) g

    val parse : (t, 'st) p =
     fn g => (parse_line_and_headers >>-
              str "\r\n" >>>
              (scanAnyChars >>@ (fn "" => NONE | s => SOME s)) >>-
              eos >>@ (fn ((l,hs),body) =>
                          {line=l,
                           headers=hs,
                           body=body})
             ) g

    val methodToString =
     fn OPTIONS => "OPTIONS"
      | GET => "GET"
      | HEAD => "HEAD"
      | POST => "POST"
      | PUT => "PUT"
      | DELETE => "DELETE"
      | TRACE => "TRACE"
      | CONNECT => "CONNECT"

    fun lineToString {method,uri,version} =
        String.concatWith " " [methodToString method,
                               Uri.toString uri,
                               Version.toString version]

    fun toString {line,headers,body} =
        let val line = lineToString line
            val body = case body of NONE => "" | SOME b => b
        in String.concatWith "\r\n"
                             (line ::
                              foldr (fn (h,a) => Header.toString h :: a)
                                    ["",body] headers)
        end

    (* Multi-part form data *)

    datatype mpfd =
       File_mpfd of { name          : string,
                      filename      : string,
                      content       : substring,
                      content_types : (string*string) list
                    }
     | Norm_mpfd of { name          : string,
                      content       : substring,
                      content_types : (string*string) list
                    }

    val skipWs : (unit,'st) p =
     fn g => skipChars Char.isSpace g

    val scan_boundary : (string,'st) p =
     fn g => (skipWs ->>
              str "multipart/form-data;" ->>
              skipWs ->>
              str "boundary=" ->>
              scanChars (fn c => c <> #"\r" andalso c <> #"\n")) g

    structure SS = Substring

    fun splitByString (sep:string) (s:substring) : substring list =
        let fun loop (s:substring) (acc:substring list) : substring list =
                if SS.isEmpty s then List.rev acc
                else let val (pref,suff) = SS.position sep s
                         val acc = if SS.isEmpty pref then acc
                                   else pref::acc
                     in if SS.isEmpty suff then List.rev acc
                        else loop (SS.slice(suff,size sep,NONE))
                                  acc
                     end
        in loop s nil
        end

    fun extractBoundary s =
        case scan_boundary SS.getc (SS.full s) of
            SOME (b,st) => if SS.isEmpty st then SOME b else NONE
          | NONE  => NONE

    fun splitData boundary data =
        let fun fixFirstPart ps =
                case ps of
                    nil => nil
                  | p::ps =>
                    case splitByString ("--" ^ boundary ^ "\r\n") p of
                        [y] => y::ps
                      | _ => p::ps
            fun fixLastPart ps =
                case List.rev ps of
                    nil => nil
                 |  p :: ps =>
                    case splitByString ("\r\n--" ^ boundary ^ "--\r\n") p of
                        [y] => List.rev (y::ps)
                      | _ => List.rev (p::ps)
        in splitByString ("\r\n--" ^ boundary ^ "\r\n") data
                         |> fixFirstPart
                         |> fixLastPart
        end

    val parse_rest : (substring,substring) p =
     fn g => fn ss => SOME (ss,SS.full "")

    val parse_id : (string, 'st) p =
     fn g => scanChars (fn c => Char.isPrint c andalso c <> #";"
                                andalso c <> #"=") g

    fun clean s = SS.full s
               |> SS.dropl Char.isSpace
               |> SS.dropr Char.isSpace
               |> (fn s => if SS.size s >= 2 andalso
                              (SS.isPrefix "\"" s andalso SS.isSuffix "\"" s
                               orelse SS.isPrefix "'" s andalso SS.isSuffix "'" s)
                           then CharVectorSlice.subslice(s,1,SOME(SS.size s - 2))
                           else s
                  )
               |> SS.string

    val parse_kv : (string*string,'st) p =
     fn g =>
        (skipWs ->>
         parse_id >>-
         str "=" >>-
         skipWs >>>
         parse_id >>@ (fn (k,v) => (clean k,
                                    clean v))
        ) g

    val rec parse_kvs : (Header.t list, 'st) p =
     fn g => ((parse_kv >>@ (fn a => [a]) >>? (str ";" ->> parse_kvs)) (fn (a,b) => a@b)
             ) g

    fun parse_disp (cts,ss) : (mpfd, 'st) p =
        fn g => (skipWs ->>
                 str "form-data;" ->>
                 parse_kvs ??
                 (fn kvs =>
                     case (Header.look kvs "name",
                           Header.look kvs "filename") of
                         (SOME name, SOME filename) =>
                         SOME(File_mpfd {name=name,
                                         filename=filename,
                                         content=ss,
                                         content_types=cts})
                       | (SOME name, NONE) =>
                         SOME(Norm_mpfd {name=name,
                                         content=ss,
                                         content_types=cts})
                       | _ => NONE
                 )) g

    val parseFD : (mpfd,substring) p =
     fn g =>
        (parse_headers >>-
         str "\r\n" >>>
         parse_rest ??
         (fn (headers, ss:substring) =>
             case Header.look headers "Content-Disposition" of
                 NONE => NONE
               | SOME disp =>
                 let val cts = List.filter (fn (k,v) => Header.keyEq "Content-Type" k) headers
                 in case parse_disp (cts,ss) SS.getc (SS.full disp) of
                        SOME(x,_) => SOME x
                      | NONE => NONE
                 end)
        ) g

    fun parseMPFD {contentType} : substring -> mpfd list option =
        fn ss =>
           case extractBoundary contentType of
               NONE => NONE
             | SOME boundary =>
               let val parts = splitData boundary ss
               in foldr (fn (part, NONE) => NONE
                          | (part, SOME acc) =>
                            case parseFD SS.getc part of
                                SOME (mpfd, ss) =>
                                if SS.isEmpty ss then SOME (mpfd::acc)
                                else NONE
                              | NONE => NONE)
                        (SOME nil) parts
               end

    fun dataFromString (data:string) : (string * string) list =
      let val tokens = String.tokens (fn c => c = #"&") data
      in List.foldr (fn (t,acc)=>
                        case String.tokens (fn c => c = #"=") t of
                            [k,v] => (k,Util.decodeUrl v)::acc
                          | nil => acc
                          | k::_ => (k,"")::acc) nil tokens
      end

  end

  structure Response = struct
    type line = {version: Version.t, status: StatusCode.t}
    type t = {line:line, headers: (string*string)list, body: string option}

    val parse_line : (line,'st) p =
     fn g => (Version.parse >>-
              p_space >>>
              StatusCode.parse >>-
              p_space >>-
              skipChars (fn c => c <> #"\r") >>@
              (fn (v,sc) => {version=v,
                             status=sc})
             ) g

    val parse : (t, 'st) p =
     fn g => (parse_line >>-
              str "\r\n" >>>
              Request.parse_headers >>-
              str "\r\n" >>>
              (scanAnyChars >>@ (fn "" => NONE | s => SOME s)) >>-
              eos >>@ (fn ((l,hs),body) =>
                          {line=l,
                           headers=hs,
                           body=body})
             ) g

    fun lineToString {version, status} =
        String.concat [Version.toString version, " ",
                       StatusCode.toString status, " ",
                       StatusCode.reason status]

    fun toString {line, headers, body} =
        let val line = lineToString line
            val body = case body of NONE => "" | SOME b => b
        in String.concatWith "\r\n"
                             (line ::
                              foldr (fn (h,a) => Header.toString h :: a)
                                    ["", body] headers)
        end
  end

end
