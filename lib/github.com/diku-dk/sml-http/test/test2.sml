
fun test s p e f =
    let val () = print (s ^ ": ")
        val res = f ()
    in if res = e then print "OK\n"
       else print ("ERR - expecting '" ^ p e ^ "' - got '" ^ p res ^ "'\n")
    end
    handle Fail msg => print ("EXN Fail(" ^ msg ^ ") - expected '" ^ p e ^ "'\n")
         | ? => print ("EXN - expected '" ^ p e ^ "' - " ^ General.exnMessage ? ^ "\n")

fun id x = x

open Http.Util

val () = test "url-encode-1" id "abc" (fn () => encodeUrl "abc")
val () = test "url-encode-2" id "a%20bc" (fn () => encodeUrl "a bc")

val () = test "url-decode-1" id "abc" (fn () => decodeUrl "abc")
val () = test "url-decode-2" id "a bc" (fn () => decodeUrl "a%20bc")
val () = test "url-decode-3" id "a bc%" (fn () => decodeUrl "a%20bc%")
val () = test "url-decode-4" id "a bc%x" (fn () => decodeUrl "a%20bc%x")
val () = test "url-decode-5" id "a bc%ax a" (fn () => decodeUrl "a%20bc%ax%20a")

fun test_urlencode s = test "url-deencode" id s (fn () => decodeUrl(encodeUrl s))

val () = app test_urlencode ["", "abc", "sdfj3wewe", " efd eed & /(982s"]

(* Multi-part form-data *)

val contentType = "multipart/form-data; boundary=----WebKitFormBoundaryksWgGlysQDIBSQfv"

val data =
    String.concatWith "\r\n"
 ["------WebKitFormBoundaryksWgGlysQDIBSQfv",
  "Content-Disposition: form-data; name=\"file\"; filename=\"test1.mlb\"",
  "Content-Type: application/octet-stream",
  "",
  "local",
  "  $(SML_LIB)/basis/basis.mlb",
  "  ../server.mlb",
  "in",
  "  test1.sml",
  "end",
  "",
  "------WebKitFormBoundaryksWgGlysQDIBSQfv",
  "Content-Disposition: form-data; name=\"file\"; filename=\"test2.out.ok\"",
  "Content-Type: application/octet-stream",
  "",
  "url-encode-1: OK",
  "url-encode-2: OK",
  "url-deencode: OK",
  "Ending",
  "",
  "------WebKitFormBoundaryksWgGlysQDIBSQfv--",
  ""]

val l = Http.Request.parseMPFD {contentType=contentType}
                               (Substring.full data)

fun prPart p =
    let open Http.Request
    in case p of
           File_mpfd {name,filename,content,...} =>
           print ("name=" ^ name ^ "; filename=" ^ filename ^ "; size(content)=" ^
                  Int.toString (Substring.size (content)) ^ "\n")
         | Norm_mpfd {name,content,...} =>
           print ("name=" ^ name ^ "; size(content)=" ^
                  Int.toString (Substring.size (content)) ^ "\n")
    end

val () = case l of
             SOME l =>
             ( print ("MPFD: length(l) = " ^ Int.toString (length l) ^ "\n")
             ; app prPart l
             )
           | NONE =>
             print "MPFD: err\n"

val kvs = Http.Request.dataFromString "first=this+is+a+field&second=was+it+clear+%28already%29%3F"

val () = test "dataFromString-1" (fn x => Option.getOpt(x,""))
              (SOME"this is a field")
              (fn () => Http.Header.look kvs "first")

val () = test "dataFromString-2" (fn x => Option.getOpt(x,""))
              (SOME"was it clear (already)?")
              (fn () => Http.Header.look kvs "second")
