%% ======================== RetMIP ================================
% Script that compares modelled and observed firn density
% Work in progress...
% Contact me if any question: b.vandecrux@gmail.com
% =================================================================

clear all
close all

set(0,'defaultfigurepaperunits','centimeters');
set(0,'DefaultAxesFontSize',15)
set(0,'defaultfigurecolor','w');
set(0,'defaultfigureinverthardcopy','off');
set(0,'defaultfigurepaperorientation','landscape');
set(0,'defaultfigurepaperpositionmode','auto');
set(0,'DefaultTextInterpreter','none');
set(0, 'DefaultFigureUnits', 'centimeters');
set(0, 'DefaultFigurePosition', [.25 .25 [29.7 16]-0.5]);
set(0,'defaultfigurepapersize',[29.7 16]);

% make sure you have the following folder in your path
% it contains all the matlab functions needed for the analysis
addpath(genpath('..\lib'))

load Core_RetMIP.mat
% the Core structure contains a lot of info about each core. Check it out!

% Print list of all cores
CoreList(Core)

% Consider looping on station names or on filenames
station = 'KAN-U';
filename = 'C:\Users\bava\Dropbox\Phd\Data release\RetMIP\Output example\KAN-U\RetMIP_GEUS_KAN-U_3hourly_columns.nc';
OutputFolder = './Plots';
mkdir(OutputFolder)

vis = 'on'; % if 'off' figure will be generated and saved but not displayed 

% loading nc file
finfo = ncinfo(filename);
names={finfo.Variables.Name};
for i= 1:size(finfo.Variables,2)
    % eval is dangerous... use with care
    eval(sprintf('%s = ncread(''%s'',''%s'');', char(names{i}), filename,char(names{i})));
end

% using Matlab native time stamps
time_mod = time + datenum(1900,1,0);
depth2 = [0; depth];

% Cores have a tag called NearestCodeLocation that can take either 'Summit'
% 'KAN-U' or 'FA'
% here we find the indexes of the cores for a specific station
i_core = FindCore(Core,'NearestCodeLocation',station);
    
    
% ordering cores in chronological order
dates = zeros(size(i_core));
for i = 1:length(i_core)
    dates(i) = datenum(Core{i_core(i)}.Info.DateCored);
end
[~, i_ordered] = sort(dates);
i_core = i_core(i_ordered);
    
% removing the cores that were drilled more than 1 year before (resp. after) the
% beginning (resp. end) of the model run
i_remove = [];
for i = 1:length(i_core)
    DV = datevec(time_mod);
    if Core{i_core(i)}.Info.DateCored.Year < DV(1,1) - 1
        i_remove = [i_remove, i];
    end
    if Core{i_core(i)}.Info.DateCored.Year > DV(end,1) + 1
        i_remove = [i_remove, i];
    end
end
i_core(i_remove) = [];
    
%% Plotting

num_plot=6;
f = figure('Visible',vis,'outerposition',[1 1 25 15]);
[ha, ~] = tight_subplot(1, num_plot, 0.03, [0.12 0.2], [0.08 0.01]);
count = 0;
ylim_core = 20;

for ii = i_core
    count = count+1;
    
    % start another figure if the current one is filled
    if count <= num_plot
        set(f,'CurrentAxes',ha(count))
    else
        % Increment name of files rather than overwriting
        i_file = 1;
        NameFile = sprintf('%s/CorePlot%i.pdf',OutputFolder, i_file);
        while exist(NameFile, 'file') == 2
            i_file = i_file + 1;
            NameFile = sprintf('%s/CorePlot%i.pdf',OutputFolder,i_file)  ;
        end
        print(f,NameFile,'-dpdf');
        if strcmp(vis,'off')
            close(f);
        end
        f = figure('Visible',vis);
        [ha, ~] = tight_subplot(1, num_plot, 0.03, [0.12 0.2], [0.08 0.01]);
        count = 1;
        set(f,'CurrentAxes',ha(count))
    end

    time_core = datenum(Core{ii}.Info.DateCored);

    % find closest time step in the model
    temp = abs(time_mod - time_core);
    [~, ind_time] = min(temp);
    
    % beware of "stairs" it needs density along with the top depth of each 
    % interval whereas RetMIP densities are given along with the bottom depth
    % of each interval
    density = [rho(:,ind_time); rho(end,ind_time)];
    stairs(density,-depth2, 'Linewidth',1.5)
    hold on
    % in the core dataset depth are in cm
    stairs([Core{ii}.Data.Density; Core{ii}.Data.Density(end)], ...
        -[0; Core{ii}.Data.Depth]/100, 'Linewidth',1.5)
    
    ylim([-ylim_core 0])
    title(sprintf('%s \n %s', Core{ii}.Info.Name, ...
        datestr(datenum(Core{ii}.Info.DateCored))), ... % datetime format is very unhandy, I know...
        'Interpreter','none',...
        'FontSize',12);
    set(gca,'TickLength',get(gca,'TickLength')*2)
    if count == 1
        xlabel('Density (kg/m^3)')
        ylabel('Depth (m)')
        legendflex({'Modelled','Observed'},'ref',gcf,'anchor',{'n' 'n'},'nrow',1)
    else
        set(gca,'YTickLabel',' ')
    end
end

for i = (count+1):num_plot
    set(f,'CurrentAxes',ha(i))
    set(gca,'Visible','off')
end

% Increment name of files rather than overwriting
i_file = 1;
NameFile = sprintf('%s/CorePlot%i.pdf',OutputFolder, i_file);
while exist(NameFile, 'file') == 2
    i_file = i_file + 1;
    NameFile = sprintf('%s/CorePlot%i.pdf',OutputFolder,i_file)  ;
end
print(f,NameFile,'-dpdf');

if strcmp(vis,'off')
    close(f);
end
