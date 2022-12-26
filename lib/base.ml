let with_file_in path f =
  let channel = open_in path in
  try f channel
  with x ->
    close_in channel;
    raise x
