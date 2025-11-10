function y = ignition(x)
  x = x(:,1:10);
  r = sqrt(sum(x.^2, 2));
  t = 200000*normcdf(sqrt(2).*10.*(r-2));
  y = log10(r.^5.*(1 + t));
  y = (y - 3.9)./2.5;
end
