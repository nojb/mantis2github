(* This is free and unencumbered software released into the public domain.

   Anyone is free to copy, modify, publish, use, compile, sell, or
   distribute this software, either in source code form or as a compiled
   binary, for any purpose, commercial or non-commercial, and by any
   means.

   In jurisdictions that recognize copyright laws, the author or authors
   of this software dedicate any and all copyright interest in the
   software to the public domain. We make this dedication for the benefit
   of the public at large and to the detriment of our heirs and
   successors. We intend this dedication to be an overt act of
   relinquishment in perpetuity of all present and future rights to this
   software under copyright law.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.

   For more information, please refer to <http://unlicense.org> *)

module Labels : sig
  val list: ?verbose:bool -> ?token:string -> string * string -> string list
  val create: ?verbose:bool -> ?token:string -> string * string -> string -> int -> unit
end

module Milestones : sig
  val list: ?verbose:bool -> ?token:string -> string * string -> (string * int) list
  val create: ?verbose:bool -> ?token:string -> string * string -> string -> int
end

module Assignees : sig
  val list: ?verbose:bool -> ?token:string -> string * string -> string list
end

module Issue : sig
  module Issue : sig
    type t =
      {
        title: string;
        body: string;
        created_at: string option;
        closed_at: string option;
        updated_at: string option;
        assignee: string option;
        milestone: int option;
        closed: bool option;
        labels: string list;
      }
  end

  module Comment : sig
    type t =
      {
        created_at: string option;
        body: string;
      }
  end

  type t =
    {
      issue: Issue.t;
      comments: Comment.t list;
    }

  val to_json: t -> Yojson.Basic.t

  type res =
    | Imported of int
    | Failed
    | Pending

  val import: ?verbose:bool -> ?token:string -> string * string -> t -> int
  val check_imported: ?verbose:bool -> ?token:string -> string * string -> int -> res
  val exists: ?verbose:bool -> ?token:string -> string * string -> int -> bool
end

module Gist : sig
  type t =
    {
      files: (string * string) list;
      description: string;
      public: bool;
    }

  val create: ?verbose:bool -> ?token:string -> t -> (string * string) list
  val last: ?verbose:bool -> ?token:string -> unit -> (string * string) option
  val delete: ?verbose:bool -> ?token:string -> string -> unit
end
