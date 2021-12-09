structure Http :> HTTP = struct

  open ScanUtil
  infix >>> ->> >>- >>? || >>@ >>* ??

  fun scanAnyChar get s = get s

 (* memo: instead of building a list, we should be able to extract the string from the
    underlying slice *)

  fun scanAnyChars get =
      (list scanAnyChar >>@ implode) get

  val p_space : (unit,'st) p =
   fn g => ign (scanChar (fn c => c = #" ")) g

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
    val BadRequest = "400"
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

    val parse : (t, 'st) p =
     fn g => (parse_line >>-
              str "\r\n" >>>
              parse_headers >>-
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
  end

  structure Response = struct
    type line = {version: Version.t, status: StatusCode.t}
    type t = {line:line, headers: (string*string)list, body: string option}
    (*  val parse    : (t, 'st) p *)
    fun lineToString {version, status} =
        String.concatWith " " [Version.toString version,
                               StatusCode.toString status,
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
