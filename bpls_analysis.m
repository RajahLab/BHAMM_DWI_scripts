%% bpls_analysis.m 
% Authors: Sricharana Rajagopal, Rikki Lissaman
% Last updated (dd/mm/yyyy): 02/01/2024
%
% Description: This MATLAB Script runs behavior partial least squares (PLS) 
% to examine whether sex/menopause status affects the association beteen 
% chronological age and JHU ROI-specific measures of white matter 
% microstructure (fractional anisotropy [FA], mean diffusivity [MD]). It
% also exports values used for subsequent analysis and plotting.
%
% Inputs (per analysis):
%   (1) A text file containing participant IDs and ROI-specific measures of
%   FA.
%   (2) A text file containing participant IDs and ROI-specific measures of
%   MD.
%   (3) A text file containing participant ages (standardized).
% Outputs (per analysis): 
%   (1) A csv file containing values for the singular profile (LV1), which 
%   can be plotted in R.
%   (2) A csv file containing LV1 brain score values.
%
% Note(s):
%   (1) The command-line version of the Rotman-Baycrest PLS application
%   must be downloaded and added to the path. For more information regarding 
%   the application, see: http://pls.rotman-baycrest.on.ca/UserGuide.htm
%   (2) To facilitate understanding, the code includes values used for the
%   menopause analysis (pre-meno N = 32, post-meno N = 34).
%   (3) Where IDs are used, they have been replaced with generic numbers
%   (e.g., 0001) as placeholders. 
%   (4) Input files should be stacked by group. For example, in the
%   menopause analysis, the text file containing FA data should have
%   pre-menopausal females first and post-menopausal females second. The
%   code below will then stack this so the datamat is stacked by
%   condition/metric and then group. The behavioral text file has to mimic 
%   this, even if values must be duplicated.

% Load various ROI-specific metrics of microstructure (FA, MD)
conditions={'FA', 'MD'};
filename_prefix='input-text-file-prefix'; % text files must have same prefix

for c=1:numel(conditions)
    T{c} = readtable(strcat(filename_prefix, '-',lower(conditions{c}), '.txt'));
    T{c}.metric=repmat(conditions{c}, size(T{c},1),1);
    
    % Pre-menopausal group
    rows=false(size(T{c},1),1);
    rows(1:32)=true; % specify rows containing data for group 1 (pre)
    T{c}.group(rows)= repmat({'1_Pre'}, 32, 1); % second value should specify N of first group (pre)
    
    % Post-menopausal group
    rows=false(size(T{c},1),1);
    rows(33:66)=true; % specify rows containing data for group 2 (post)
    T{c}.group(rows)= repmat({'2_Post'}, 34, 1); % second value should specify N of second group (post)

end

table_all = vertcat(T{:});
table_all = sortrows(table_all, 24); % value reflects last column (i.e., the one containing group labels)

%% Set up datamat
currDir=pwd;

% List participant IDs for each group
group_pre={0001, 0002, .., 0032};
group_post={0033, 0034, .., 0066};

groups={'1_Pre', '2_Post'}; % set groups as labels specified above
subjectList={group_pre, group_post};

nsubj=cellfun('length', subjectList);
ncond=numel(conditions);
ngroup=numel(subjectList);

% Change 2:end-2 on line 78 to 1:end-2 if ID is NOT included as column 1 in
% the text files used as input
for gg =1:ngroup
    rowidx=find(strcmp(table_all.group, groups{gg}) == 1);
    datamat{gg} = table2array(table_all(rowidx, 2:end-2)); 
end

% Load behaviour vector, stored as a text file (in this case, the text file
% should contain standardized age values with the same structure as the
% microstructure measures datamat)
behav_vector=load('input-file.txt');

%% Options to set before running PLS analysis
% method: This option will decide which PLS method that the program will use:
%   1. Mean-Centering Task PLS
%	2. Non-Rotated Task PLS
%	3. Regular Behavior PLS
%	4. Regular Multiblock PLS
%	5. Non-Rotated Behavior PLS
%	6. Non-Rotated Multiblock PLS
option.method = 3; % more information can be found in pls_analysis.m

% num_boot = This option sets the number of bootstraps
option.num_boot = 1000; 

% num_perm = This option sets the number of permutations
option.num_perm = 2000;

% Need to set for behavior PLS
option.stacked_behavdata=behav_vector;

% Specify filename for results.mat
outFileName='output-file'; 

% Run behavior PLS
disp('Running PLS analysis...');
result = pls_analysis(datamat,nsubj,ncond,option);

% Save result.mat to current working directory
disp('Saving result file...');
save(strcat(currDir, '/', outFileName, '.mat'), 'result');

%% Looking at results
% Plot p-values
pval = result.perm_result.sprob
nLV=numel(pval);
figure;
bar(pval,'r');
hold on;
h = zeros(nLV,1);
for i=1:nLV
    h(i)=plot(NaN,NaN, '.r');
end
legend(h,strcat('LV', num2str([1:nLV]'), {' - '} ,num2str(pval)));
title(['Permuted values greater than observed, ', num2str(option.num_perm), ' permutation tests']);
hold off;

% Plot effect sizes (% crossblock covariance)
pcov = result.s.^2 / sum(result.s.^2)
figure;
bar(pcov);
hold on;
h = zeros(nLV,1);
for i=1:nLV
    h(i)=plot(NaN,NaN, '.');
end
legend(h,strcat('LV', num2str([1:nLV]'), {' - '} ,num2str(pcov*100), '%'));
title('Percent Crossblock covariance');
hold off;

%% Plot Correlation Overview/Task PLS scores
lv=1;
load('rri_color_code');

figure;
if option.method == 3 || option.method == 5
    numOfBehavVecs=size(result.stacked_behavdata,2);
    upperLim=result.boot_result.ulcorr-result.boot_result.orig_corr;
    lowerLim=result.boot_result.orig_corr-result.boot_result.llcorr;
    barResult=result.boot_result.orig_corr;
    for g=1:ngroup
        for k=1:ncond
            bar_hdl = bar((g-1)*numOfBehavVecs*ncond + [1:numOfBehavVecs] + numOfBehavVecs*(k-1), ...
                barResult((g-1)*numOfBehavVecs*ncond + [1:numOfBehavVecs] + numOfBehavVecs*(k-1), ...
                lv)); hold on;
            set(bar_hdl,'facecolor',color_code(k,:));
        end
    end
    %     errorbar(1:size(barResult,2),barResult(:,lv), lowerLim(:,lv),upperLim(:,lv), 'k.'); hold off;
    if size(barResult,1) ~= size(barResult,2)
        errorbar(1:size(barResult,1),barResult(:,lv), lowerLim(:,lv),upperLim(:,lv), 'k.'); hold off;
    else
        errorbar(1:size(barResult,2),barResult(:,lv), lowerLim(:,lv),upperLim(:,lv), 'k.'); hold off;
    end
    y_label='Correlations';
    plot_title='Correlations Overview ';
else
    upperLim=result.boot_result.ulusc-result.boot_result.orig_usc;
    lowerLim=result.boot_result.orig_usc-result.boot_result.llusc;
    barResult=result.boot_result.orig_usc;

    for g=1:ngroup
        for k=1:ncond
            bar_hdl = bar((g-1)*ncond + k,barResult((g-1)*ncond + k,lv)); hold on;
            set(bar_hdl,'facecolor',color_code(k,:));

        end
    end
    errorbar(1:size(barResult,2),barResult(:,lv), lowerLim(:,lv),upperLim(:,lv), 'k.'); hold off;
    y_label='Brain Scores';
    plot_title='Task PLS Brain Scores with CI ';
end

[l_hdl, o_hdl] = legend(conditions); % matlab 2019 version
legend_txt(o_hdl);
set(l_hdl,'color',[0.9 1 0.9]);
setappdata(gca,'LegendHdl2',[{l_hdl} {o_hdl}]);

set(gca, 'XGrid', 'on');
set(gca, 'GridLineStyle', '--');

xlabel('Groups');
ylabel(y_label);
if option.method == 1
    set(gca,'XTick',([2:ngroup] - 1)*ncond +0.5)
elseif option.method ==3
    set(gca,'XTick',([2:ngroup] - 1)*ncond*numOfBehavVecs +0.5)
end
set(gca,'XTickLabel',1:ngroup);
title([plot_title, 'of LV: ', num2str(lv)]);

%% Extract values to analyze/plot in R 

% Assuming LV1 is significant, export the original correlation and its
% lower/upper limits to a new data matrix, sing_profile. This will be used
% to create a barplot for the singular profile
sing_profile(:,1) = result.boot_result.orig_corr(:, 1);
sing_profile(:,2) = result.boot_result.llcorr(:, 1);
sing_profile(:,3) = result.boot_result.ulcorr(:, 1);

% Export the matrix as a csv file
writematrix(sing_profile, "output-file.csv")

% Assuming LV1 is significant, export the "brain scores" linked to the LV
lv1_brainscores = result.usc(:, 1)

% Export the matrix as a csv file
writematrix(lv1_brainscores, "output-file.csv")