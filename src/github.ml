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

module J = Yojson.Basic.Util

module Api = struct
  type t =
    | POST
    | GET
    | DELETE

  let to_string = function
    | GET -> "GET"
    | POST -> "POST"
    | DELETE -> "DELETE"

  let root = "https://api.github.com"

  let curl meth ?(verbose = false) ?(headers = []) ?data ?(params = []) ?token route =
    let headers =
      match token with
      | Some token -> ("Authorization", "token " ^ token) :: headers
      | None -> headers
    in
    let headers = ("Accept", "application/vnd.github.golden-comet-preview+json") :: headers in
    let tmp = Filename.temp_file "curl" "out" in
    let cmd =
      let headers =
        match headers with
        | [] -> ""
        | l ->
            String.concat ""
              (List.map (fun (k, v) -> Printf.sprintf "-H \"%s: %s\" " k v) l)
      in
      let data =
        match data with
        | None -> ""
        | Some data ->
            let filename, oc = Filename.open_temp_file "curl" "data" in
            set_binary_mode_out oc true;
            Yojson.Basic.pretty_to_channel ~std:true oc data;
            close_out oc;
            "--data-binary @" ^ filename ^ " "
      in
      let params =
        match params with
        | [] -> ""
        | l -> "?" ^ String.concat "&" (List.map (fun (k, v) -> String.concat "=" [k; v]) l)
      in
      let verbose = if false && verbose then "--verbose " else "" in
      Printf.sprintf "curl -L -w '%%{http_code}\\n' -Ss %s%s%s-X %s '%s%s%s' -o %s"
        verbose data headers (to_string meth) root route params tmp
    in
    if verbose then Printf.eprintf "+ %s\n%!" cmd;
    let tmp_http_code = Filename.temp_file "curl" "code" in
    match Sys.command (Printf.sprintf "%s > %s" cmd tmp_http_code) with
    | 0 ->
        let ic = open_in tmp_http_code in
        let code = input_line ic |> int_of_string in
        close_in ic;
        let ic = open_in_bin tmp in
        let s = really_input_string ic (in_channel_length ic) in
        close_in ic;
        let json = Yojson.Basic.from_string s in
        if code >= 400 then begin
          let json = ["cmd", `String cmd; "res", json] in
          let json = match data with None -> json | Some data -> ("data", data) :: json in
          let json = `Assoc json in
          Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
          Printf.ksprintf failwith "Github API call failed: %s" route
        end;
        json
    | _ ->
        Printf.ksprintf failwith "Could not execute command: %S" cmd

  let get ?verbose ?headers ?params ?token fmt =
    Printf.ksprintf (curl GET ?verbose ?headers ?params ?token) fmt

  let post ?verbose ?headers ?data ?token fmt =
    Printf.ksprintf (curl POST ?verbose ?headers ?data ?token) fmt

  let delete ?verbose ?headers ?token fmt =
    Printf.ksprintf (fun s -> ignore (curl DELETE ?verbose ?headers ?token s)) fmt
end

module Labels = struct
  let list ?verbose ?token (owner, repo) =
    let params = ["per_page", "100"] in
    Api.get ?verbose ?token ~params "/repos/%s/%s/labels" owner repo
    |> J.to_list
    |> List.map (J.member "name")
    |> List.map J.to_string
end

module Assignees = struct
  let list ?verbose ?token (owner, repo) =
    let params = ["per_page", "100"] in
    Api.get ?verbose ?token ~params "/repos/%s/%s/assignees" owner repo
    |> J.to_list
    |> List.map (J.member "login")
    |> List.map J.to_string
end

let str s1 s2 l =
  match s2 with
  | None -> l
  | Some s2 -> (s1, `String s2) :: l

let bool s1 s2 l =
  match s2 with
  | None -> l
  | Some s2 -> (s1, `Bool s2) :: l

let int s1 s2 l =
  match s2 with
  | None -> l
  | Some s2 -> (s1, `Int s2) :: l

module Issue = struct
  module Issue = struct
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

    let to_json
        {title; body; created_at; closed_at; updated_at;
         assignee; milestone; closed; labels}
      =
      let l = ("labels", `List (List.map (fun s -> `String s) labels)) :: [] in
      let l = bool "closed" closed l in
      let l = int "milestone" milestone l in
      let l = str "assignee" assignee l in
      let l = str "updated_at" updated_at l in
      let l = str "closed_at" closed_at l in
      let l = str "created_at" created_at l in
      let l = ("body", `String body) :: l in
      let l = ("title", `String title) :: l in
      `Assoc l
  end

  module Comment = struct
    type t =
      {
        created_at: string option;
        body: string;
      }

    let to_json {created_at; body} =
      let l = str "created_at" created_at [] in
      let l = ("body", `String body) :: l in
      `Assoc l
  end

  type t =
    {
      issue: Issue.t;
      comments: Comment.t list;
    }

  let to_json {issue; comments} =
    `Assoc ["issue", Issue.to_json issue; "comments", `List (List.map Comment.to_json comments)]

  type res =
    | Imported of int
    | Failed
    | Pending

  let check_imported ?verbose ?token (owner, repo) id =
    let json = Api.get ?verbose ?token "/repos/%s/%s/import/issues/%d" owner repo id in
    match json |> J.member "status" |> J.to_string with
    | "imported" ->
      let id =
        json
        |> J.member "issue_url"
        |> J.to_string
        |> String.split_on_char '/'
        |> List.rev
        |> List.hd
        |> int_of_string
      in
      Imported id
    | "failed" -> Failed
    | "pending" -> Pending
    | _ ->
      assert false

  let import ?verbose ?token (owner, repo) issue =
    let data = to_json issue in
    let json = Api.post ?verbose ~data ?token "/repos/%s/%s/import/issues" owner repo in
    json |> J.member "id" |> J.to_int

  let exists ?verbose ?token (owner, repo) id =
    match Api.get ?verbose ?token "/repos/%s/%s/issues/%d" owner repo id with
    | exception _ -> false
    | _ -> true
end

module Gist = struct
  type t =
    {
      files: (string * string) list;
      description: string;
      public: bool;
    }

  let to_json {files; description; public} =
    let files =
      List.map (fun (name, contents) ->
          name, `Assoc ["content", `String contents]
        ) files
    in
    `Assoc [ "files", `Assoc files;
             "description", `String description;
             "public", `Bool public ]

  let exec ~verbose cmd =
    if verbose then Printf.eprintf "+ %s\n%!" cmd;
    assert (0 = Sys.command cmd)

  let clone_and_push ?(verbose = false) ?(token = "") id files =
    let exec = exec ~verbose in
    let dir = Filename.get_temp_dir_name () in
    Printf.ksprintf exec "rm -rf %s" (Filename.concat dir id);
    Printf.ksprintf exec "git -C %s clone https://%s@gist.github.com/%s.git" dir token id;
    let dir = Filename.concat dir id in
    Printf.ksprintf exec "git -C %s rm '*'" dir;
    (* Printf.ksprintf exec "git -C %s config user.name $(git -C %s show --format=%%an -s HEAD)" *)
    (*   dir dir; *)
    (* Printf.ksprintf exec "git -C %s config user.name $(git -C %s show --format=%%ae -s HEAD)" *)
    (*   dir dir; *)
    List.iter (fun (name, content) ->
        let oc = open_out_bin (Filename.concat dir name) in
        output_string oc content;
        close_out oc;
        Printf.ksprintf exec "git -C %s add '%s'" dir name
      ) files;
    Printf.ksprintf exec "git -C %s commit --amend --allow-empty-message --no-edit" dir;
    Printf.ksprintf exec "git -C %s push -f" dir

  let dummy description =
    {
      files = ["hello", "world"];
      description;
      public = true;
    }

  let is_ascii s =
    let rec loop i =
      if i >= String.length s then
        true
      else begin
        match s.[i] with
        | '\x00'..'\x7F' -> loop (succ i)
        | _ -> false
      end
    in
    loop 0

  let create ?verbose ?token gist =
    let is_ascii = List.for_all (fun (_, contents) -> is_ascii contents) gist.files in
    let data = to_json (if is_ascii then gist else dummy gist.description) in
    let json = Api.post ?verbose ~data ?token "/gists" in
    let json =
      if is_ascii then
        json
      else begin
        let id = J.member "id" json |> J.to_string in
        clone_and_push ?verbose ?token id gist.files;
        Api.get ?verbose ?token "/gists/%s" id
      end
    in
    J.member "files" json |> J.to_assoc |>
    List.map (fun (_, json) ->
        let filename = json |> J.member "filename" |> J.to_string in
        let raw_url = json |> J.member "raw_url" |> J.to_string in
        filename, raw_url
      )

  let last ?verbose ?token () =
    let json = Api.get ?verbose ?token "/gists" in
    match J.to_list json with
    | [] -> None
    | x :: _ ->
        let description = J.member "description" x |> J.to_string in
        let gist_id = J.member "id" x |> J.to_string in
        Some (description, gist_id)

  let delete ?verbose ?token gist_id =
    Api.delete ?verbose ?token "/gists/%s" gist_id
end
