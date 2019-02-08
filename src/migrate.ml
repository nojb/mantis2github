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

let badd buf title s =
  let fence = String.make 6 '`' in
  let s = String.trim s in
  if s <> "" then Printf.bprintf buf "**%s**\n%s\n%s\n%s\n" title fence s fence

module Note : sig
  val migrate: Mantis.Note.t -> Github.Issue.Comment.t
end = struct
  let migrate {Mantis.Note.reporter; text; last_modified = _; date_submitted} =
    let reporter = match reporter with None -> "" | Some s -> s in
    let text =
      let buf = Buffer.create 101 in
      badd buf "Reporter" reporter;
      badd buf "Body" text;
      Buffer.contents buf
    in
    {Github.Issue.Comment.body = text;
     created_at = Some date_submitted}
end

module Issue : sig
  val migrate: gh_user:(string -> string option) -> Mantis.Issue.t -> Github.Issue.t
end = struct
  let body ~id ?(reporter = "") ~tags ~category ~version ~target_version ~fixed_in_version
      ~priority ~severity
      ~description ~steps_to_reproduce ~additional_information ~related:_
    =
    let buf = Buffer.create 101 in
    let info =
      let combine l =
        let l = List.map (fun (s1, s2) -> (s1, String.trim s2)) l in
        let l = List.filter (function (_, "") -> false | _ -> true) l in
        String.concat "\n" (List.map (fun (s1, s2) -> s1 ^ ": " ^ s2) l)
      in
      combine
        [ "ID", string_of_int id;
          "Reporter", reporter;
          "Version", version;
          "Target version", target_version;
          "Fixed in version", fixed_in_version;
          "Category", category;
          "Priority", Mantis.Priority.to_string priority;
          "Severity", Mantis.Severity.to_string severity;
          "Tags", String.concat ", " tags ];
    in
    badd buf "Original bug information" info;
    badd buf "Description" description;
    badd buf "Steps to reproduce" steps_to_reproduce;
    badd buf "Additional information" additional_information;
    Buffer.contents buf

  let labels ~priority ~severity ~category:_ ~status:_ ~resolution =
    let l =
      Mantis.Priority.to_labels priority @
      Mantis.Severity.to_labels severity @
      Mantis.Resolution.to_labels resolution
    in
    List.sort_uniq Stdlib.compare l |> List.map Mantis.Label.to_string

  let milestone ~target_version:_ =
    None

  let migrate ~gh_user
      {
        Mantis.Issue.id;
        summary;
        priority;
        severity;
        category;
        date_submitted = created_at;
        last_updated = updated_at;
        reporter;
        handler = assignee;
        description;
        steps_to_reproduce;
        additional_information;
        version;
        target_version;
        fixed_in_version;
        notes;
        status;
        closed_at;
        resolution;
        related;
        tags;
      }
    =
    let title = if summary = "" then "*no title*" else summary in
    let body =
      body ~id ?reporter ~tags ~category ~version ~target_version ~fixed_in_version
        ~priority ~severity
        ~description ~steps_to_reproduce ~additional_information ~related
    in
    let labels = labels ~priority ~severity ~category ~status ~resolution in
    let milestone = milestone ~target_version in
    let closed = Mantis.Status.is_closed status in
    let closed_at =
      match closed_at, closed with
      | None, true -> Some updated_at
      | None, false -> None
      | Some _ as x, _ -> x
    in
    let assignee =
      match assignee with
      | Some s ->
          gh_user s
      | None ->
          None
    in
    let issue =
      {Github.Issue.Issue.title; body;
       created_at = Some created_at;
       updated_at = Some updated_at;
       assignee; milestone; closed_at;
       closed = Some closed; labels}
    in
    {Github.Issue.issue; comments = List.map Note.migrate notes}
end
