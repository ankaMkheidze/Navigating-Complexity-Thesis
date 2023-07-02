function [X, t] = generate_data( n, noise)
%GENERATE_DATA Generates an artificial high dimensional dataset (manifold)
%
%	[X,t] = generate_data(n, noise)
%
% high-dimensional dataset in X. In
% addition, the function returns the coordinates of the datapoints on the
% underlying manifold in t.
%
% Very important run rgl(123) before you run this function this sets the
% seed!
%
% Note in order to generate the 5000 observations value 6000 is used for n
% which produces 7776 rows of observations. Then this data set is cut to
% 5000. An excel format of data will also be attached!
%
% This file is part of the Matlab Toolbox for Dimensionality Reduction.
% The toolbox can be obtained from http://homepage.tudelft.nl/19j49
%


	if ~exist('n', 'var')
		n = 1000;
    end
    if ~exist('noise', 'var')
        noise = 0.05;
    end
            % Generate underlying manifold
            no_dims = 5;
            no_points_per_dim = round(n ^ (1 / no_dims));
            l = linspace(0, 1, no_points_per_dim);
            t = combn(l, no_dims);
            
            % Generate high-dimensional dataset
            X = [cos(t(:,1)) tanh(3 * t(:,2)) t(:,1) + t(:,3) t(:,4) .* sin(t(:,2)) sin(t(:,1) + t(:,5)) t(:,5) .* cos(t(:,2)) t(:,5) + t(:,4) t(:,2) t(:,3) .* t(:,4) t(:,1)];
            X = X + noise * randn(size(X));

	end
