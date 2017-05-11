Horner <- function(x, coefs)
{
  count = length(coefs);
  res = coefs[count];
  for (i in (count-1):1)
    res = res*x + coefs[i];
  return ( res );
}

ConstantTrend <- function(values)
{
  count = length(values);
  res = vector(length = length(values));
  sum = sum(values)/count;
  res[1:count] = sum;
  return ( res );
}

LinearTrend <- function(values)
{
  count = length(values);
  sum1 = 0;
  sum2 = 0;
  for (i in 1:count)
  {
    sum1 = sum1 + i*values[i];
    sum2 = sum2 + values[i];
  }
  b1 = (((sum1) - ((count+1)/2)*sum2))/( (count)*(count^2 - 1)/12);
  b0 = sum(values)/count - b1*(count+1)/2;
  coefs = c(b0, b1);
  return ( Horner(c(1:count), coefs) );
}

StandardDeviation <- function(values, trendFunction)
{
  count = length(values);
  balancedVals = trendFunction(values);
  sum = 0;
  for (i in 1:count)
    sum = sum + (values[i] - balancedVals[i])^2;
  return ( (sum/(count-2))^(1/2) );
}


