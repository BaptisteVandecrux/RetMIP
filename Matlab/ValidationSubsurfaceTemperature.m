%% ======================== RetMIP ================================
% Script that compares modelled and observed firn density
% At the moment it only processes the output from GEUS at KAN-U. It should
% take much to expend it to all the other models/sites.
%
% Baptiste Vandecrux
% b.vandecrux@gmail.com
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
addpath(genpath('.\lib'))
addpath(genpath('.\Data'))


% Consider looping on station names or on filenames
station = 'KAN-U';
filename = 'RetMIP_GEUS_KAN-U_3hourly_columns.nc';
OutputFolder = './Output';

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
T_ice_mod = temp;

%% Loading observed subsurface temperature
filename = 'data_KAN-U_PROMICE_Tice.txt';
[time_obs, T_ice_obs, ...
    depth_thermistor, Surface_Height, data_out] = ...
    ExtractTice(filename,station);


%% Ploting observed temperature
 f=figure('Visible',vis);%('outerposition',[1 -1  25 25]);
 set(gcf,'Position',[0    1.0583   36.1421   17.2508])

    ylimit = 20;
    step = 72;

    TT = repmat(time_obs',size(depth_thermistor,1),1);

    % here is a cool function. Check its header to know more about it!
    col = PlotTemp(TT(:, 1:step:end),...
        depth_thermistor(:, 1:step:end),...
        T_ice_obs(:, 1:step:end),...
        'PlotTherm', 'no',...
        'PlotIsoTherm', 'no',...
        'ShowLegend','no',...
        'cmap','jet',...
        'Interp','on',...
        'XLabel','Year',...
        'YLabel','Depth (m)',...
        'CLabel','Observed firn temperature (^oC)',...
        'Range', -40:1:0);
    
title(station)
    temp = col.YTickLabel;
    for k = 1:length(temp)
        if (k-1)/5==floor((k-1)/5)
        else
            temp(k,:)=' ';
        end
    end
    col.YTickLabel=temp;
    
print(f, sprintf('%s/T_ice_obs',OutputFolder), '-dpng')
    if strcmp(vis,'off')
        close(f)
    end
    
%% Ploting modelled temperature
 f=figure('Visible',vis);%('outerposition',[1 -1  25 25]);    

    depth_mod = repmat(depth,1,length(time_mod));
    TT_mod = ones(size(depth_mod,1),1) * time_mod';

    step = 72;
    col = PlotTemp([TT_mod(1, 1:step:end); TT_mod(:, 1:step:end)],...
        [zeros(size(time_mod(1:step:end)')); depth_mod(:, 1:step:end)],...
        [T_ice_mod(1, 1:step:end); T_ice_mod(:, 1:step:end)]-273.15,...
        'PlotTherm', 'no',...
        'PlotIsoTherm', 'no',...
        'ShowLegend','no',...
        'cmap','jet',...
        'Interp','on',...
        'XLabel','',...
        'YLabel',' ',...
        'CLabel','Modelled firn temperature (^oC)',...
        'Range', -40:1:0);
    title(station)

    temp = col.YTickLabel;
    for k = 1:length(temp)
        if (k-1)/5==floor((k-1)/5)
        else
            temp(k,:)=' ';
        end
    end
    col.YTickLabel=temp;


	    print(f, sprintf('%s/T_ice_mod',OutputFolder), '-dpng')
    if strcmp(vis,'off')
        close(f)
    end
    
%% Ploting temperature difference

%  f=figure('Visible',vis);%('outerposition',[1 -1  25 25]);
% 
%     TT = ones(size(depth_thermistor,1) * time_mod');
%     step = 24;
    
error(['I stop here: Modelled and observed firn temperatures are on \n' ...
    'different depth and time grids. Some resampling will be necessary.'])

% Subsurface temperature bias
% T_diff = T_subsurf_mod - T_ice_obs{ii};

%     col = PlotTemp(TT(:, 1:step:end),...
%         depth_thermistor_m{ii}(:, 1:step:end),...
%         T_diff(:, 1:step:end),...
%         'PlotTherm', 'no',...
%         'PlotIsoTherm', 'no',...
%         'ShowLegend','no',...
%         'cmap','BWR_cmap',...
%         'Interp','on',...
%         'XLabel','',...
%         'YLabel',' ',...
%         'CLabel','Firn temperature (^oC)',...
%         'Range', -10:10);
% %     col.FontSize = 12;
%    
%     temp = col.YTickLabel;
%     for k = 1:length(temp)
%         if (k-1)/5==floor((k-1)/5)
%         else
%             temp(k,:)=' ';
%         end
%     end
%     col.YTickLabel=temp;
% 
% 	    print(f, sprintf('%s/T_ice_diff',OutputFolder), '-dpng')
%     if strcmp(vis,'off')
%         close(f)
%     end
    
