-- Source - https://stackoverflow.com/a/58679929
-- Posted by mb21, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-12-11, License - CC BY-SA 4.0

function Div(el)
  if el.classes[1] == 'hidden' then
    return {}
  else
    return el
  end
end
