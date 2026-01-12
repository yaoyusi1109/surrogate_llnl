function y = trayfunc(x)

  x1 = x(:,1).*4 - 2;
  x2 = x(:,2).*4 - 2;
  
  p1 = abs(100 - sqrt(x1.^2 + x2.^2)./pi);
  p2 = abs(sin(x1).*sin(x2).*exp(p1)) + 1;
  y = -0.0001.*(p2).^0.1;
  y = (y + 1.9)./0.2;
  
end
