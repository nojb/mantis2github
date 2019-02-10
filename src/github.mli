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

module Api : sig
  val get:
    ?verbose:bool -> ?headers:(string * string) list -> ?params:(string * string) list ->
    ?token:string -> ('a, unit, string, Yojson.Basic.t option) format4 -> 'a

  val post:
    ?verbose: bool -> ?headers:(string * string) list -> ?data:Yojson.Basic.t ->
    ?token:string -> ('a, unit, string, Yojson.Basic.t option) format4 -> 'a
end

module Label : sig
  val list: ?verbose:bool -> ?token:string -> owner:string -> repo:string -> unit -> string list option
end

module Milestone : sig
  val list: ?verbose:bool -> ?token:string -> owner:string -> repo:string -> unit -> (string * int) list option
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

    val to_json: t -> Yojson.Basic.t
  end

  module Comment : sig
    type t =
      {
        created_at: string option;
        body: string;
      }

    val to_json: t -> Yojson.Basic.t
  end

  type t =
    {
      issue: Issue.t;
      comments: Comment.t list;
    }

  val to_json: t -> Yojson.Basic.t

  val import: ?verbose:bool -> ?token:string -> owner:string -> repo:string -> t -> (int * int, int) result
  val list: ?verbose:bool -> ?token:string -> owner:string -> repo:string -> unit -> int list option
end

module Gist : sig
  type t =
    {
      files: (string * string) list;
      description: string;
      public: bool;
    }

  val to_json: t -> Yojson.Basic.t
  val create: ?verbose:bool -> ?token:string -> t -> string option
end
