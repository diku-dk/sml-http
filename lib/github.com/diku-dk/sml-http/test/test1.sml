

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

(* Uri *)

fun testUri s uri =
    test s id uri (fn () => Uri.toString(fromString Uri.parse uri))

val () = testUri "uri-1a" "http://di.ku.dk/"

val () = testUri "uri-1b" "https://di.ku.dk/"

val () = testUri "uri-2" "http://di.ku.dk:8000/"

val () = testUri "uri-3" "http://di.ku.dk:8000/hello.sml"

val () = testUri "uri-4" "http://di.ku.dk:8000/hello.sml?hej"

val () = testUri "uri-5" "http://di.ku.dk/?hej"

val () = testUri "uri-6" "http://ku.dk/?hej"

val () = testUri "url-7" "*"

val () = testUri "url-8" "/?hej"

val () = testUri "url-9" "/"

fun testUri_noslash s uri =
    test s id (uri ^ "/") (fn () => Uri.toString(fromString Uri.parse uri))

val () = testUri_noslash "uri-10" "http://di.ku.dk"

val () = testUri_noslash "uri-11" "https://di.ku.dk"

val () = testUri_noslash "uri-12" "https://di.ku.dk:8000"

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

val h1s = "sec-ch-ua: \" Not A;Brand\";v=\"99\", \"Chromium\";v=\"96\", \"Google Chrome\";v=\"96\""
val h1t = "sec-ch-ua:\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"96\", \"Google Chrome\";v=\"96\""

val () = test "header-4" id h1t (fn () => Header.toString(fromString Header.parse h1s))

val h2t = "Accept:text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"

val h2s = "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"

val () = test "header-5" id h2t (fn () => Header.toString(fromString Header.parse h2s))

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

val () = test_line "line-1" "GET http://di.ku/ HTTP/1.1"

val () = test_line "line-2" "HEAD http://di.ku:8000/index.html?q=s HTTP/1.0"

fun test_req s e =
    test s id e (fn () => Request.toString(fromString Request.parse e))

val () = test_req "req-1" "POST http://di.ku/ HTTP/1.1\r\nKey1:Value1\r\nKey2:Value2\r\n\r\nHere is some form data"

val () = test_req "req-2" "POST / HTTP/1.1\r\nKey1:Value1\r\nKey2:Value2\r\n\r\nHere is some form data"

val () = test_req "req-3" "POST /ok.sml?a=34 HTTP/1.1\r\nKey1:Value1\r\nKey2:Value2\r\n\r\nHere is some form data"


fun test_req_headerslack msg s t =
    test msg id t (fn () => Request.toString(fromString Request.parse s))

val () = test_req_headerslack "req-10"
     "POST http://di.ku/ HTTP/1.1\r\nKey1:   Value1 \r\nKey2: Value2  \r\n\r\nHere is some form data"
     "POST http://di.ku/ HTTP/1.1\r\nKey1:Value1\r\nKey2:Value2\r\n\r\nHere is some form data"

val () = test_req_headerslack "req-11"
     "POST http://di.ku:8000/ HTTP/1.1\r\n\r\nHere is some form data"
     "POST http://di.ku:8000/ HTTP/1.1\r\n\r\nHere is some form data"

val () = test_req_headerslack "req-12"
     "DELETE http://di.ku:8000/data?id=8 HTTP/1.0\r\n\r\n"
     "DELETE http://di.ku:8000/data?id=8 HTTP/1.0\r\n\r\n"

val () = test_req_headerslack "req-13"
     "DELETE /data?id=8 HTTP/1.0\r\n\r\n"
     "DELETE /data?id=8 HTTP/1.0\r\n\r\n"
