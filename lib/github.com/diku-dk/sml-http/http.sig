signature HTTP = sig

  type ('a,'st) p = ('a,'st) ScanUtil.p

  structure Util : sig
     val decodeUrl : string -> string
     val encodeUrl : string -> string
     val buildUrl  : string -> (string * string) list -> string
  end

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

    val OK                  : t   (* 200 *)
    val Redirect            : t   (* 302 *)
    val BadRequest          : t   (* 400 *)
    val Forbidden           : t   (* 403 *)
    val NotFound            : t   (* 404 *)
    val LengthRequired      : t   (* 411 *)
    val InternalServerError : t   (* 500 *)
  end

  structure Header : sig
    type t = string * string
    val parse    : (t, 'st) p
    val look     : t list -> string -> string option (* case insensitive *)
    val lookAll  : t list -> string -> string list   (* case insensitive *)
    val keyEq    : string -> string -> bool          (* case insensitive *)
    val toString : t -> string
  end

  structure Request : sig
    datatype method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

    type line = {method: method, uri: Uri.t, version: Version.t}
    type t = {line:line, headers: Header.t list, body: string option}

    val parse_method           : (method, 'st) p
    val parse_headers          : (Header.t list, 'st) p
    val parse_line             : (line, 'st) p
    val parse_line_and_headers : (line * Header.t list, 'st) p
    val parse                  : (t, 'st) p
    val methodToString         : method -> string
    val lineToString           : line -> string
    val toString               : t -> string

    (* Url-encoded form data *)
    val dataFromString         : string -> Header.t list

    (* Multi-part form-data *)
    datatype mpfd =
        File_mpfd of { name          : string,
                       filename      : string,
                       content       : substring,
                       content_types : Header.t list
                     }
      | Norm_mpfd of { name          : string,
                       content       : substring,
                       content_types : Header.t list
                     }

    val parseMPFD             : {contentType: string} -> substring
                                -> mpfd list option
  end

  structure Response : sig
    type line = {version: Version.t, status: StatusCode.t}
    type t = {line: line, headers: Header.t list, body: string option}
    val parse_line   : (line, 'st) p
    val parse        : (t, 'st) p
    val lineToString : line -> string
    val toString     : t -> string
  end

end
