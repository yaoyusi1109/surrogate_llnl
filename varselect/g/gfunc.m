function y = gfunc(x, a)
  n = size(x, 1);
  d = size(x, 2);

  % default for a
  if (nargin == 1)
      a = zeros(1, d);
      for ii = 1:d
          a(ii) = (ii-1) / 2;
      end
  end

  prod = ones(n, 1);
  for i = 1:d
    prod = prod .* (abs(4*x(:,i)-2) + a(i))/(1 + a(i));
  end

  y = prod;

end
