-module(stickyNotes_webresource).
-export([init/1, process_post/2, allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) -> {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.


process_post(ReqData, Context) ->
    Ps = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Struct = mochijson2:decode(proplists:get_value("json", Ps)),
    Act = list_to_existing_atom(binary_to_list(struct:get_value(<<"action">>, Struct))), 
    Resp = wrq:merge_resp_headers([{"Content-Type", "application/json"}], ReqData),
    Resp2 = wrq:append_to_response_body(mochijson2:encode(notes:Act(Struct)), Resp),
    {true, Resp2, Context}.                