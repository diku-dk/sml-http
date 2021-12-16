signature HTTP = sig

  type ('a,'st) p = ('a,'st) ScanUtil.p

  structure Uri : sig
    datatype t = URL of {scheme: string, host: string,
                         port: int option, path: string,
                         query: string}   (* http://domain:port/path?query *)
               | AST
               | PATH of {path: string,
                          query: string}  (* /path?query *)
    val parse    : (t, 'st) p
    val toString : t -> string
  end

  structure Mime : sig
    type t
    val parse      : (t, 'st) p
    val toString   : t -> string
    val fromString : string -> t option
    val fromExt    : string -> t option
  end

  structure Version : sig
    eqtype t
    val parse    : (t, 'st) p
    val toString : t -> string
    val HTTP_1_0 : t
    val HTTP_1_1 : t
  end

  structure StatusCode : sig
    eqtype t           (* 3 digits *)
    val reason     : t -> string
    val fromString : string -> t option
    val parse      : (t, 'st) p
    val toString   : t -> string

    val OK                  : t
    val BadRequest          : t
    val InternalServerError : t
  end

  structure Header : sig
    type t = string * string
    val parse    : (string*string, 'st) p
    val toString : t -> string
  end

  structure Request : sig
    datatype method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

    type line = {method: method, uri: Uri.t, version: Version.t}
    type t = {line:line, headers: (string*string)list, body: string option}

    val parse_method    : (method, 'st) p
    val parse_headers   : ((string*string)list, 'st) p
    val parse_line      : (line, 'st) p
    val parse           : (t, 'st) p
    val methodToString  : method -> string
    val lineToString    : line -> string
    val toString        : t -> string
  end

  structure Response : sig
    type line = {version: Version.t, status: StatusCode.t}
    type t = {line: line, headers: (string*string)list, body: string option}
    val parse_line   : (line, 'st) p
    val parse        : (t, 'st) p
    val lineToString : line -> string
    val toString     : t -> string
  end

end
