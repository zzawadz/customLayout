
layColRepGenerator <- function(side)
{
  if(side == "widths") fnc <- layColBind
  if(side == "heights") fnc <- layRowBind
  
  function(layout, n)
  {
    layall <- layout
    if(n > 1) for(i in 2:n)
    {
      size <- c(i-1,1)
      layall <- fnc(layall,layout, size)
    }
    layall
  }
}

layRepByCol <- layColRepGenerator("widths")
layRepByRow <- layColRepGenerator("heights")
