structure Http :> HTTP = struct

  open ScanUtil
  infix >>> ->> >>- >>? || >>@ >>* ?? >??

  (* op >?? : ('a,'st) p * ('b,'st) p -> ('a * 'b option, 'st) p *)
  fun (p1 >?? p2) g s =
      case p1 g s of
          SOME (a,s) =>
          (case p2 g s of
               SOME (b,s) => SOME((a,SOME b),s)
             | NONE => SOME((a,NONE),s))
        | NONE => NONE

  fun repeat p g s =
      let fun loop s acc =
              case p g s of
                  NONE => SOME(rev acc,s)
                | SOME(e,s) => loop s (e::acc)
      in loop s nil
      end

  val p_space : (unit,'st) p =
   fn g => ign (scanChar (fn c => c = #" ")) g

  structure Url = struct
    type t = {scheme: string, host: string,
              port: int option, path: string,
              query: string}   (* http://domain:port/path?query *)

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

    val parse : (t, 'st) p =
     fn g =>
       ((p_scheme >>- str "://" >>> p_host) >??
        (str ":" ->> p_port) >??
        con ("/",()) >??
        p_path >??
        p_query ??
        (fn (((((s, h), port), sep), path), query) =>
            let val path = case (sep, path) of
                               (SOME(), SOME p) => SOME ("/" ^ p)
                             | (_, NONE) => SOME "/"
                             | (NONE, SOME _) => NONE
            in case path of
                   NONE => NONE
                 | SOME path =>
                   SOME {scheme=s,host=h,port=port,path=path,
                         query=Option.getOpt(query,"")}
            end)) g

    fun toString {scheme, host, port, path, query} =
        scheme ^ "://" ^ host ^
        (case port of
             SOME i => ":" ^ Int.toString i
           | NONE => "") ^
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
  end

  structure StatusCode = struct
    open HttpStatus
    val parse : (t, 'st) p =  (* 3 digits *)
     fn g => (scanChars Char.isDigit ?? fromString) g
  end

  structure Header = struct
    type t = string * string
    val parse : (string*string, 'st) p =
     fn g => (scanChars (fn c => Char.isPrint c andalso
                                 c <> #":") >>-
              str ":" >>>
              skipWS(scanChars (fn c => Char.isPrint c andalso
                                        not(Char.isSpace c))) >??
              scanChars (fn c => Char.isSpace c andalso
                                 c <> #"\n" andalso
                                 c <> #"\r") >>@
              (#1)) g
    fun toString (n,v) =
        n ^ ":" ^ v
  end

  structure Request = struct
    datatype method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

    type line = {method: method, version: Version.t, url: Url.t}
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
     fn g => repeat (Header.parse >>- str "\r\n") g

    val parse_line : (line, 'st) p =
     fn g => (parse_method >>- p_space >>>
              Version.parse >>- p_space >>>
              Url.parse >>@
              (fn ((m,v),u) => {method=m,version=v,url=u})) g
    val parse : (t, 'st) p =
     fn g => (parse_line >>- str "\r\n" >>>
              parse_headers >??
              (str "\r\n" ->> scanChars (fn _ => true) >>- eos)
              >>@ (fn ((l,hs),b) => {line=l,headers=hs,body=b})) g

    val methodToString =
     fn OPTIONS => "OPTIONS"
      | GET => "GET"
      | HEAD => "HEAD"
      | POST => "POST"
      | PUT => "PUT"
      | DELETE => "DELETE"
      | TRACE => "TRACE"
      | CONNECT => "CONNECT"

    fun lineToString {method,version,url} =
        String.concatWith " " [methodToString method,
                               Version.toString version,
                               Url.toString url]

    fun toString {line,headers,body} =
        String.concatWith "\r\n" [lineToString line,
                                  String.concatWith "\r\n" (map Header.toString headers),
                                  "",
                                  case body of NONE => "" | SOME b => b]

  end

  structure Response = struct
    type t = {version: Version.t, status: StatusCode.t,
              headers: (string*string)list, body: string option}
(*  val parse    : (t, 'st) p *)
    fun toString {version, status, headers, body} =
        let val line =
                String.concatWith " " [Version.toString version,
                                       StatusCode.toString status,
                                       StatusCode.reason status]
            val headers = String.concatWith "\r\n" (map Header.toString headers)
            val body = case body of NONE => "" | SOME b => b
        in String.concatWith "\r\n" [line, headers, "", body]
        end
  end

end
