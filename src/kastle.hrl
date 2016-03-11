%%%
%%%   Copyright (c) 2016, Klarna AB
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%

-ifndef(__KASTLE_HRL).
-define(__KASTLE_HRL, true).

-include_lib("lager/include/lager.hrl").

-define(KASTLE_JSON_SCHEMA, kastle_json_schema).

-define(MESSAGE_KEY, <<"key">>).
-define(MESSAGE_VALUE, <<"value">>).

-define(BROD_CLIENT, kastle_kafka_client).

-endif. % include kastle.hrl

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
