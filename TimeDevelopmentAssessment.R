AbsoluteDifference <- function(values, order = 1)
{
  if (order < length(values))
  {
    count = length(values);
    for (i in 1:order)
    {
      tmpTimes = values;
      for (j in i:(count-1))
        values[j+1] = tmpTimes[j+1] - tmpTimes[j];
    }
    return (values[(order+1):count])
  }
}

AbsoluteDifferenceAverage <- function(values)
{
  count = length(values);
  return ((values[count] - values[1]) / (count-1));
}

GrowthRate <- function(values, periodSize = 1)
{
  count = length(values);
  rates = vector(length = count-periodSize);
  for (i in (periodSize+1):count)
  {
    rates[i-periodSize] = values[i] / values[i-periodSize];
  }
    
  return ( rates );
}

GrowthRateAverage <- function(values)
{
  count = length(values);
  return ( (values[count]/values[1])^(1/(count-1)));
}

RelativeIncrement <- function(values)
{
  count = length(values)
  increments = vector(length = count-1)
  for (i in (2:count))
    increments[i-1] = (values[i]/values[i-1]) - 1
  return ( increments )
}