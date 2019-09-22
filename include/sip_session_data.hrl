-record(data, {
  socket        :: port(),
  endpoint_ip   :: term(),
  endpoint_port :: integer(),
  host_port     :: integer(),
  from_tag      :: binary(),
  to_tag        :: binary(),
  branch        :: binary(),
  call_id       :: binary(),
  real_name     :: binary(),
  username      :: string(),
  password      :: string(),
  realm         :: string(),
  period_ms     :: non_neg_integer(),
  c_sec         :: integer(),
  number        :: binary(),
  delay         :: integer(),
  %% TODO: Delete it after migration from gen_server to statem phone
  post_invite   :: boolean()
}).
