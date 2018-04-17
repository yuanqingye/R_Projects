f = function(){
  x = 1
  y = x+1
  d = 2
  sum = 0
  for(i in 1:50){
    sum = sum+i
  }
  return(x/y+d)
}

g = function(){
  return(f())
}

error = (function () 
{
  .rs.recordTraceback(TRUE)
})()

e=function() for(i in 1:length(x)){z[i]=x[i]+y[i]}