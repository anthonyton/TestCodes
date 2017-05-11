SimpleAverage <- function(values)
{
  return ( sum(values)/length(values) )
}

ChronologicalConstantAverage <- function(values)
{
    count = length(values);
    sum = (values[1] + values[count])/2;
    for (i in 2:(count-1))
      sum = sum + values[i];
    return ( sum/(count) );
}

ChronologicalIrregularAverage <- function(values, times)
{
  count = length(values);
  sum = 0;
  for (i in 1:(count-1))
    sum = sum + (values[i] + values[i+1])*(times[i]);
  return ( (sum/2) / sum(times) );
}
