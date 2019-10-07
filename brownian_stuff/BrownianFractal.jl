using Plots;
using DataFrames;
using Distributions;
X = readtable("/Users/inuzm/Documents/Rhist/molecule.txt");
Y = convert(Array, X);
    
function brownianxy(n, Time, x, y)
        vtime = range(0, stop = Time, length = n + 1);
        brown = [0; cumsum(rand(Normal(0, sqrt(Time/n)), n))];
        aux = brown[end] + x - y;
        bridge =  brown - vtime .* aux ./ Time;
        bridge = bridge .+ x;
        return bridge
end
            
function rarebrown(Time, Y)
    index = range(1, stop = 100001, length = 1459);
    Z = zeros(1459, 2);
    Z[:,1] = Y[:, 1] + rand(Normal(0, 0.1), 1459);
    Z[:,2] = Y[:, 2] + rand(Normal(0, 0.1), 1459);
    x = zeros(100001);
    y = zeros(100001);
    for i in 1:1458
        x[Integer(floor(index[i])):Integer(floor(index[i+1]))] = brownianxy(Integer(floor(index[i+1]))-Integer(floor(index[i])), Time * (Integer(floor(index[i+1] - Integer(floor(index[i])))))/100000, Z[i, 1], Z[i+1, 1]);
        y[Integer(floor(index[i])):Integer(floor(index[i+1]))] = brownianxy(Integer(floor(index[i+1]))-Integer(floor(index[i])), Time * (Integer(floor(index[i+1] - Integer(floor(index[i])))))/100000, Z[i, 2], Z[i+1, 2]);
    end
    return x, y
end

x, y = rarebrown(1, Y);
p = plot(x, y, axis=false, grid=false, label="", background_color_subplot="#073642", linecolor="azure", la=0.002, dpi = 300, size = (500, 432), lw = 0.1)
for i = 1:300
    x, y = rarebrown(1, Y);
    plot!(p, x, y, axis=false, grid=false, label="", background_color_subplot="#073642", linecolor="azure", la=0.002, lw = 0.1)
end
plot!(p, Y[:,1], Y[:,2], lw=0.5, axis=false, grid=false, label="", background_color_subplot="#073642", linecolor = "azure2", la=0.3)
png(p, "/Users/inuzm/Documents/whatevs/graphs/molecule8.png")

x = brownianxy(Integer(1e4), 1, 0, 0);
p = plot(x, axis=false, grid=false, label="", linecolor="black", la=0.1)
for i = 1:100
    x = brownianxy(Integer(1e4), 1, 0, 0);
    plot!(p, x, axis=false, grid=false, label="", linecolor="black", la=0.1)
end
p

x, y = rarebrown(10, Y);
s = scatter(x, y, axis=false, grid=false, label="", background_color_subplot="#073642", ma = 0.01, ms = 2, mc = "azure", markerstrokealpha = 0.1, markerstrokecolor = "azure", dpi = 300, size = (1500, 1296))
for i = 1:10
    x, y = rarebrown(10, Y);
    scatter!(s, x, y, axis=false, grid=false, label="", background_color_subplot="#073642", ma = 0.01, ms = 2, mc = "azure", markerstrokealpha = 0.1, markerstrokecolor = "azure")
end
