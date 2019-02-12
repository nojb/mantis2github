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

  let to_string = function
    | GET -> "GET"
    | POST -> "POST"

  let root = "https://api.github.com"

  let curl meth ?(verbose = false) ?(headers = []) ?data ?(params = []) ?token route =
    let headers =
      match token with
      | Some token -> ("Authorization", "token " ^ token) :: headers
      | None -> headers
    in
    let headers = ("Accept", "application/vnd.github.golden-comet-preview+json") :: headers in
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
      let verbose = if verbose then "--verbose " else "" in
      Printf.sprintf "curl -L -w '%%{http_code}\\n' -Ss %s%s%s-X %s '%s%s%s'"
        verbose data headers (to_string meth) root route params
    in
    if verbose then Printf.eprintf "+ %s\n%!" cmd;
    let tmp = Filename.temp_file "curl" "out" in
    let tmp_http_code = Filename.temp_file "curl" "code" in
    match Sys.command (Printf.sprintf "%s -o %s > %s" cmd tmp tmp_http_code) with
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
          None
        end else
          Some json
    | _ ->
        None

  let get ?verbose ?headers ?params ?token fmt =
    Printf.ksprintf (curl GET ?verbose ?headers ?params ?token) fmt

  let post ?verbose ?headers ?data ?token fmt =
    Printf.ksprintf (curl POST ?verbose ?headers ?data ?token) fmt
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

  type waiting = int * Yojson.Basic.t * int

  type token =
    | Failed
    | Success of int
    | Waiting of waiting

  let to_json {issue; comments} =
    `Assoc ["issue", Issue.to_json issue; "comments", `List (List.map Comment.to_json comments)]

  let check_imported ?verbose ?token ~owner ~repo (id, data, sleep) =
    Unix.sleep sleep;
    match Api.get ?verbose ?token "/repos/%s/%s/import/issues/%d" owner repo id with
    | None -> Failed
    | Some json ->
        begin match json |> J.member "status" |> J.to_string with
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
            Success id
        | "failed" ->
            let json = `Assoc ["issue", data; "error", json] in
            Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
            Failed
        | "pending" ->
            Waiting (id, data, 2 * sleep)
        | _ ->
            Failed
        end

  let import ?verbose ?token ~owner ~repo issue =
    let data = to_json issue in
    match Api.post ?verbose ~data ?token "/repos/%s/%s/import/issues" owner repo with
    | None ->
        None
    | Some json ->
        let id = json |> J.member "id" |> J.to_int in
        Some (id, data, 1)

  let count ?verbose ?token ~owner ~repo () =
    let query =
      Printf.sprintf {|
query {
  repository(owner:"%s", name:"%s") {
    pullRequests {
      totalCount
    }
    issues {
      totalCount
    }
  }
}
    |} owner repo
    in
    let data = `Assoc ["query", `String query] in
    match Api.post ?verbose ?token ~data "/graphql" with
    | None -> None
    | Some json ->
        let json =
          json
          |> J.member "data"
          |> J.member "repository"
        in
        let pr_count = json |> J.member "pullRequests" |> J.member "totalCount" |> J.to_int in
        let issue_count = json |> J.member "issues" |> J.member "totalCount" |> J.to_int in
        Some (pr_count + issue_count)
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

  let clone ?(verbose = false) ?(token = "") id files =
    let exec = exec ~verbose in
    let dir = Filename.get_temp_dir_name () in
    Printf.ksprintf exec "git -C %s clone https://%s@gist.github.com/%s.git" dir token id;
    let dir = Filename.concat dir id in
    Printf.ksprintf exec "git -C %s rm '*'" dir;
    List.iter (fun (name, content) ->
        let oc = open_out_bin (Filename.concat dir name) in
        output_string oc content;
        close_out oc;
        Printf.ksprintf exec "git -C %s add %s" dir name
      ) files;
    Printf.ksprintf exec "git -C %s commit --amend --allow-empty-message --no-edit" dir;
    Printf.ksprintf exec "git -C %s push -f" dir

  let dummy description =
    {
      files = ["hello", "world"];
      description;
      public = true;
    }

  let create ?verbose ?token gist =
    let data = dummy gist.description |> to_json in
    match Api.post ?verbose ~data ?token "/gists" with
    | Some json ->
        let html_url = J.member "html_url" json |> J.to_string in
        let git_pull_url = J.member "git_pull_url" json |> J.to_string in
        let id = Scanf.sscanf git_pull_url "https://gist.github.com/%s@.git" (fun s -> s) in
        clone ?verbose ?token id gist.files;
        begin match Api.get ?verbose ?token "/gists/%s" id with
        | None -> None
        | Some json ->
            let files_urls =
              J.member "files" json |> J.to_assoc |>
              List.map (fun (_, json) ->
                  let filename = json |> J.member "filename" |> J.to_string in
                  let url = json |> J.member "raw_url" |> J.to_string in
                  filename, url
                )
            in
            assert (List.sort Stdlib.compare (List.map fst files_urls) =
                    List.sort Stdlib.compare (List.map fst gist.files));
            Some (html_url, files_urls)
        end
    | None ->
        None
end
