-record(state, {ets_tid :: ets:tab(),
  last_sended = undef :: string() | atom(),
  completed_handler :: file:io_device(),
  period = 0,
  pool = [] :: list()}).