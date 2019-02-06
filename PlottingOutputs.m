%% ========================= RetMIP =================================
% Script plotting all the output of a 1D experiment
%
% Baptiste Vandecrux
% b.vandecrux@gmail.com
% ===================================================================
clear all
close all

set(0,'defaultfigurepaperunits','centimeters');
set(0,'DefaultAxesFontSize',15)
set(0,'defaultfigurecolor','w');
set(0,'defaultfigureinverthardcopy','off');
set(0,'defaultfigurepaperorientation','portrait');
set(0,'defaultfigurepaperpositionmode','auto');
set(0,'DefaultTextInterpreter','none');
set(0, 'DefaultFigureUnits', 'centimeters');
set(0, 'DefaultFigurePosition', [.25 .25 [29.7 16]-0.5]);
set(0,'defaultfigurepapersize',[29.7 16]);

% make sure you have the following folder in your path
% it contains all the matlab functions needed for the analysis
addpath(genpath('.\lib'))
addpath(genpath('.\Data'))

station = {'KAN-U'};
folderlist = {'./Data/GEUS/KAN-U'};

OutputFolder = './Output';

%% Plotting surface data
for ii=1:length(folderlist)
    
     namefile_val = ...
        sprintf('%s/RetMIP_GEUS_%s_3hourly_values.nc',folderlist{ii},station{ii});
            % extract surface variables
        finfo = ncinfo(namefile_val);
        names_val={finfo.Variables.Name};
        temp = {finfo.Variables.Attributes};
        for i =1:length(temp)
            units_val{i} = temp{i}.Value;
        end
        data_val = table;
        for i= 1:size(finfo.Variables,2)
            eval(sprintf('data_val.%s = ncread(''%s'',''%s'')'';', char(names_val{i}), namefile_val,char(names_val{i})));
        end
                names_val = {names_val{2:end}};

        time_mod = data_val.time + datenum(1900,1,1);

     namefile_col = ...
        sprintf('%s/RetMIP_GEUS_%s_3hourly_columns.nc',folderlist{ii},station{ii});
            % extract surface variables
        finfo = ncinfo(namefile_col);
        names_col={finfo.Variables.Name};
        temp = {finfo.Variables.Attributes};
        for i =1:length(temp)
            units_col{i} = temp{i}.Value;
        end
        data_col = table;
        time_check = ncread(namefile_col,'time')+ datenum(1900,1,1);
        
        for i= 3:size(finfo.Variables,2)            
%             disp(sprintf('data_col.%s = ncread(''%s'',''%s'');', char(names_col{i}), namefile_col,char(names_col{i})));

            eval(sprintf('data_col.%s = ncread(''%s'',''%s'');', char(names_col{i}), namefile_col,char(names_col{i})));
        end
        names_col = {names_col{3:end}};
        
        data_col.temp = data_col.temp - 273.15;
        
        %% plotting daily columns
        cmap_list = {'jet','parula','hsv','winter','jet'};
        range = {-50:5:0, 300:50:900, 0:2:20, 0:10:70, 0:0.005:0.05};

        f = figure('Visible','off');
        ha = tight_subplot(length(names_col),1,0.03,[0.12 0.05],[0.06 0.09]);
        for i = 1:size(data_col,2)
            
                DV = datevec(time_mod);
                % starting the first full day at midnight
                i_start = find(DV(:,4)==0,1,'first');
                i_end = find(DV(:,4)==0,2,'last');
                i_end = i_end(1);

                time_new = time_mod(i_start:24:i_end)';
                edge = [time_new; time_new(end)+1];
                ind_each_day = discretize(time_mod,edge);
                
                mat_daily = NaN(size(data_col.(names_col{i}),1),length(time_new));
                ind_plot = ~isnan(ind_each_day);
                for j = 1:size(data_col.(names_col{i}),1)
                    mat_daily(j,:) = accumarray(ind_each_day(ind_plot)', data_col.(names_col{i})(j,ind_plot)',[],@mean)';
                end
    
            set(f,'CurrentAxes',ha(i))
            h= pcolor(time_new, 0.1:0.1:20,mat_daily);
            set(h, 'EdgeColor', 'none');
            set(gca,'YDir','reverse','layer','top')

            col{i} =contourcmap(cmap_list{i},range{i},'colorbar','on');
            col{i}.TickLength=col{i}.TickLength*5;
            
            for k = 2:length(col{i}.YTickLabel)-1
                third = round(length(col{i}.YTickLabel)/3);
                if and(k~=third,k~=third*2)
                    col{i}.YTickLabel(k,:)=' ';
                end
            end
            col{i}.Position(1) = 0.84;
            h_ylab = ylabel(col{i},...
                [names_col{i},'\newline  (', units_col{i+2},')'],'Interpreter','tex');
            h_ylab.HorizontalAlignment = 'center';

            set_monthly_tick(time_new)
            set(gca,'XTickLabelRotation',0)
            axis tight
            switch i
                case 1
                    title(station{ii});
                case 3
                    ylabel('Depth (m)')
                case size(data_col,2)
                    xlabel('Year')
            end
            if i<size(data_col,2)
                xlabel('')
                set(gca,'XTickLabel','')
            end
            
        end 
        for i = 1:size(data_col,2)
            colormap(ha(i),cmap_list{i})
            colormap(col{i},cmap_list{i})
        end
        print(f,sprintf('%s/%s_columns',...
            OutputFolder,station{ii}),'-dtiff');

        
        %% plotting daily values

        f = figure('Visible','off');
        ha = tight_subplot(length(names_val),1,0.02,[0.12 0.06],0.08);
        for i = 2:size(data_val,2)
            set(f,'CurrentAxes',ha(i-1))
            plot(time_mod, data_val.(names_val{i-1}),'k','LineWidth',2)
            ylabel([names_val{i-1},' (', units_val{i},')'])
            set_monthly_tick(time_mod)
                        set(gca,'XTickLabelRotation',0)
            axis tight
            if (i-1)/2 == floor((i-1)/2 )
                set(gca,'YAxisLocation','right')
            end
            if i==2
                title(station{ii})
            end
            if i<size(data_val,2)
                xlabel('')
                set(gca,'XTickLabel','')
            else
                xlabel('Year')
            end
        end       
        print(f,sprintf('%s/%s_values',...
            OutputFolder,station{ii}),'-dtiff');
end