() ->
  (input, chars) ->
    return input if isNaN chars
    return '' if chars <= 0

    if input && input.length > chars
      prefix = input.substring 0, chars/2
      postfix = input.substring input.length-chars/2, input.length
      return prefix + '…' + postfix

    return input