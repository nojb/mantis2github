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

module Priority : sig
  type t =
    | None
    | Low
    | Normal
    | High
    | Urgent
    | Immediate

  val to_string: t -> string
end

module Severity : sig
  type t =
    | Feature
    | Trivial
    | Text
    | Tweak
    | Minor
    | Major
    | Crash
    | Block

  val to_string: t -> string
end

module Resolution : sig
  type t =
    | Open
    | Fixed
    | Reopened
    | Unable_to_duplicate
    | Not_fixable
    | Duplicate
    | Not_a_bug
    | Suspended
    | Wont_fix

  val to_string: t -> string
end

module Status : sig
  type t =
    | New
    | Feedback
    | Acknowledged
    | Confirmed
    | Assigned
    | Resolved
    | Closed

  val to_string: t -> string
  val is_closed: t -> bool
end

module Note : sig
  type t =
    {
      reporter: string option;
      text: string;
      last_modified: string;
      date_submitted: string;
    }

  val to_json: t -> Yojson.Basic.t
end

module Issue : sig
  type t =
    {
      id: int;
      summary: string;
      priority: Priority.t;
      severity: Severity.t;
      category: string;
      date_submitted: string;
      last_updated: string;
      reporter: string option;
      handler: string option;
      description: string;
      steps_to_reproduce: string;
      additional_information: string;
      version: string;
      target_version: string;
      fixed_in_version: string;
      notes: Note.t list;
      status: Status.t;
      closed_at: string option;
      resolution: Resolution.t;
      related: int list;
      tags: string list;
    }

  val to_json: t -> Yojson.Basic.t
end

module Db : sig
  type t

  val use: Mysql.db -> (t -> 'a) -> 'a
  val exec: t -> (string array -> 'a * 'b) -> string -> ('a, 'b) Hashtbl.t
end

val fetch: Db.t -> (int, Issue.t) Hashtbl.t
