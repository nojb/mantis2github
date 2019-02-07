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

type t =
  {
    owner: string;
    repo: string;
    token: string option;
  }

let root = "https://api.github.com"

let curl ?(verbose = false) meth ?(headers = []) ?data ?(params = []) {owner; repo; token} route =
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
          String.concat " "
            (List.map (fun (k, v) -> "-H \"" ^ k ^ ": " ^ v ^ "\"") l) ^ " "
    in
    let data =
      match data with
      | None -> ""
      | Some data ->
          (* Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) data; *)
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
    Printf.sprintf "curl -Ss %s%s%s-X %s %s/repos/%s/%s/%s%s"
      verbose data headers meth root owner repo route params
  in
  if verbose then Printf.eprintf "+ %s\n%!" cmd;
  let tmp = Filename.temp_file "curl" "out" in
  assert (Sys.command (Printf.sprintf "%s > %s" cmd tmp) = 0);
  let ic = open_in_bin tmp in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let json = Yojson.Basic.from_string s in
  (* Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json; *)
  match J.member "message" json with
  | `String _ ->
      let json = ["cmd", `String cmd; "res", json] in
      let json = match data with None -> json | Some data -> ("data", data) :: json in
      Error (`Assoc json)
  | _ ->
      Ok json

let get ?verbose ?headers ?params gh route = curl ?verbose "GET" ?headers ?params gh route
let post ?verbose ?headers ?data gh route = curl ?verbose "POST" ?headers ?data gh route

module Milestone = struct
  let list ?verbose gh =
    let f json =
      let title = json |> J.member "title" |> J.to_string in
      let number = json |> J.member "number" |> J.to_int in
      Some (title, number)
    in
    match get ?verbose ~params:["state", "all"] gh "milestones" with
    | Error json ->
        Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
        None
    | Ok json ->
        Some (json |> J.to_list |> J.filter_map f)
end

module Issue = struct
  let is_imported ?verbose gh id =
    match get ?verbose gh (Printf.sprintf "import/issues/%d" id) with
    | Error _ as x -> Some x
    | Ok json ->
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
            Some (Ok id)
        | "failed" ->
            Some (Error json)
        | _ ->
            None
        end

  let start_import ?verbose gh json =
    match post ?verbose ~data:json gh "import/issues" with
    | Ok json -> Ok (json |> J.member "id" |> J.to_int)
    | Error _ as x -> x

  let import ?verbose gh json =
    match start_import ?verbose gh json with
    | Error json ->
        Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
        Error 0
    | Ok id ->
        let rec loop n retries =
          Unix.sleep n;
          match is_imported ?verbose gh id with
          | Some (Ok id) -> Ok (id, retries)
          | Some (Error json_err) ->
              let json = `Assoc ["issue", json; "error", json_err] in
              Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
              Error retries
          | None ->
              loop (2 * n) (succ retries)
        in
        loop 1 0
end
