%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(static_resource).
-export([init/1, allowed_methods/2,
         content_types_provided/2, resource_exists/2, last_modified/2, provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").
-record(context, {docroot,fullpath,fileinfo,response_body}).

init(DocRoot) -> {ok, #context{docroot=DocRoot}}.

resource_exists(ReqData, Context) -> 
  case get_full_path(Context, wrq:disp_path(ReqData)) of
    undefined -> {false, ReqData, Context};
    Path -> 
      case filelib:is_regular(Path) of
        true -> 
          case file:read_file_info(Path) of
            {ok, FileInfo} ->
              {true, ReqData, Context#context{fileinfo=FileInfo}};
            {error, _} ->  
              {false, ReqData, Context}
          end;    
        _ -> {false, ReqData, Context}
      end
  end.
    
content_types_provided(ReqData, Context) ->
    Path = get_full_path(Context, wrq:disp_path(ReqData)),  
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Context#context{fullpath=Path}}.

allowed_methods(ReqData, Context) -> {['HEAD', 'GET'], ReqData, Context}.  

last_modified(ReqData, Context) ->
  {(Context#context.fileinfo)#file_info.mtime, ReqData, Context}.  
 

provide_content(ReqData, Context) ->
  case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of 
    {true, NewContext} ->
      Body = NewContext#context.response_body,
      {Body, ReqData, Context};
    {false, NewContext} ->
      {error, ReqData, NewContext}
  end.  

    
% ------------------ PRIVATE ------------------------
maybe_fetch_object(Context, Path) ->
  % if returns {true, NewContext} then NewContext has response_body
  case Context#context.response_body of
    undefined ->
      case file_exists(Context, Path) of 
        {true, FullPath} ->  
          {ok, Value} = file:read_file(FullPath),
          {true, Context#context{response_body=Value}};
        false ->
          {false, Context}
      end;
    _Body ->
      {true, Context}
  end.
  
file_exists(Context, Path) ->
  FPath = get_full_path(Context, Path),
  case filelib:is_regular(filename:absname(FPath)) of 
    true ->
      {true, FPath};
    false ->
      false
  end.  
    
get_full_path(Context, Path) ->
  Root = Context#context.docroot,
  case mochiweb_util:safe_relative_path(Path) of
    undefined -> undefined;
    RelPath ->
      FullPath = filename:join([Root, RelPath]),
      case filelib:is_dir(FullPath) of
        true ->
          filename:join([FullPath, "index.html"]);
        false ->
          FullPath
      end
  end.            
