

fun test s p e f =
    let val () = print (s ^ ": ")
        val res = f ()
    in if res = e then print "OK\n"
       else print ("ERR - expecting " ^ p e ^ " - got " ^ p res ^ "\n")
    end
    handle Fail msg => print ("EXN Fail(" ^ msg ^ ") - expected " ^ p e ^ "\n")
         | _ => print ("EXN - expected " ^ p e ^ "\n")

fun id x = x

open Http

fun fromString p s =
    case p CharVectorSlice.getItem (CharVectorSlice.full s) of
        SOME (v, s) =>
        if CharVectorSlice.isEmpty s then v
        else raise Fail "expecting empty slice"
      | NONE => raise Fail "parsing failed"

(* Url *)

fun testUrl s url =
    test s id url (fn () => Url.toString(fromString Url.parse url))

val () = testUrl "url-1a" "http://di.ku.dk/"

val () = testUrl "url-1b" "https://di.ku.dk/"

val () = testUrl "url-2" "http://di.ku.dk:8000/"

val () = testUrl "url-3" "http://di.ku.dk:8000/hello.sml"

val () = testUrl "url-4" "http://di.ku.dk:8000/hello.sml?hej"

val () = testUrl "url-5" "http://di.ku.dk/?hej"

val () = testUrl "url-6" "http://ku.dk/?hej"

fun testUrl_noslash s url =
    test s id (url ^ "/") (fn () => Url.toString(fromString Url.parse url))

val () = testUrl_noslash "url-10" "http://di.ku.dk"

val () = testUrl_noslash "url-11" "https://di.ku.dk"

val () = testUrl_noslash "url-12" "https://di.ku.dk:8000"

(* Mime types *)

val () = test "mime-1" id "text/html" (fn () => Mime.toString(fromString Mime.parse "text/html"))

val () = test "mime-2" id "text/html" (fn () => case Mime.fromString "text/html" of
                                                    SOME v => Mime.toString v
                                                  | NONE => "no")

val () = test "mime-3" id "text/html" (fn () => case Mime.fromExt ".html" of
                                                    SOME v => Mime.toString v
                                                  | NONE => "no")

(* Version *)

val () = test "version-1" id "HTTP/1.0" (fn () => Version.toString(fromString Version.parse "HTTP/1.0"))

val () = test "version-2" id "HTTP/1.1" (fn () => Version.toString(fromString Version.parse "HTTP/1.1"))

(* Status codes *)

val () = test "status-1" id "200" (fn () => StatusCode.toString(fromString StatusCode.parse "200"))

val () = test "status-2" id "OK" (fn () => StatusCode.reason(fromString StatusCode.parse "200"))


(* Headers *)

val () = test "header-1" id "Key:Value" (fn () => Header.toString(fromString Header.parse "Key:Value"))

val () = test "header-2" id "Key:Value" (fn () => Header.toString(fromString Header.parse "Key:  Value"))

val () = test "header-3" id "Key:Value" (fn () => Header.toString(fromString Header.parse "Key:  Value "))


(* Requests *)

fun test_method s e =
    test s id e (fn () => Request.methodToString(fromString Request.parse_method e))

val () = test_method "method-1" "OPTIONS"
val () = test_method "method-2" "GET"
val () = test_method "method-3" "HEAD"
val () = test_method "method-4" "POST"
val () = test_method "method-5" "PUT"
val () = test_method "method-6" "DELETE"
val () = test_method "method-7" "TRACE"
val () = test_method "method-8" "CONNECT"

fun test_line s e =
    test s id e (fn () => Request.lineToString(fromString Request.parse_line e))

val () = test_line "line-1" "GET HTTP/1.1 http://di.ku/"

val () = test_line "line-2" "HEAD HTTP/1.0 http://di.ku:8000/index.html?q=s"

fun test_req s e =
    test s id e (fn () => Request.toString(fromString Request.parse e))

val () = test_req "req-1" "POST HTTP/1.1 http://di.ku/\r\nKey1:Value1\r\nKey2:Value2\r\n\r\nHere is some form data"


fun test_req_headerslack msg s t =
    test msg id t (fn () => Request.toString(fromString Request.parse s))

val () = test_req_headerslack "req-10"
     "POST HTTP/1.1 http://di.ku/\r\nKey1:   Value1 \r\nKey2: Value2  \r\n\r\nHere is some form data"
     "POST HTTP/1.1 http://di.ku/\r\nKey1:Value1\r\nKey2:Value2\r\n\r\nHere is some form data"
