using Plots, IterTools, Distributions;

function brownianxy(n, Time, x, y)
       vtime = range(0, stop = Time, length = n + 1);
       brown = [0; cumsum(rand(Normal(0, sqrt(Time/n)), n))];
       aux = brown[end] + x - y;
       bridge =  brown - vtime .* aux ./ Time;
       bridge = bridge .+ x;
       return bridge
end

X = zeros(10000, 50); Y = zeros(10000, 50);

for i in 1:50
   X[:, i] = brownianxy(9999, 1, 0, 0);
   Y[:, i] = brownianxy(9999, 1, 0, 0);
end

function sumaaa(t)
   return (1 - sqrt(t)) .* X + sqrt(t) .*Y;
end

itr = imap(t -> sumaaa(t), 0:0.1:1)

animate(itr, la = 0.1, lc = "#b2497d", axis=false, grid=false, label="", background_color_subplot="#073642", dpi = 300, "/Users/inuzm/Documents/gifs/test1.gif", fps = 2)