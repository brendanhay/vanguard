%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

%%
%% Parse Transforms
%%

%% Logging
-compile({parse_transform, lager_transform}).

%%
%% Types
%%

-type options()  :: [proplists:property()].
-type backend()  :: [string()].

%%
%% Macros
%%

-define(TIMEOUT, 2000).

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
