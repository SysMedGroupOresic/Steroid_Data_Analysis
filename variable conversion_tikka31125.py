import re
from docx import Document

# Read the content of the file
with open('new_variables.txt', 'r') as file:
    content = file.read()

# Define a dictionary with old variable names as keys and new descriptive variable names as values
variable_mapping = {
    "NAFLD": "NonAlcoholicFattyLiverDisease",
    "oknames": "columnNames",
    "groups": "steroidGroups",
    "GVZ": "groupValues",
    "ForSteroidNames": "steroidNames",
    "MASLD": "MetabolicAssociatedLiverDisease",
    "eva": "evaluationCriteria",
    "td": "targetData",
    "val": "valueList",
    "vale": "valueListAdjusted",
    "ME": "menopauseMarkers",
    "ME2": "menopauseMarkersPatients",
    "to": "patientNumbers",
    "te": "markerValues",
    "M11": "marker11KA4",
    "a": "BMI_ordered_MASLD",
    "b": "BMI_ordered_NAFLD",
    "them": "uniqueBMIValues",
    "Bali": "bileAcidsLiverData",
    "Pfase": "PFASSerumData",
    "Base": "serumBileAcidsData",
    "C4": "C4Data",
    "Clini": "clinicalData",
    "tv_all": "finalCombinedDataFrame",
    "tv": "CombinedData"
    "hep": "RelevantColumns"
    "tv_half": "HalfImputedData"
    "tv_half_log2": "Log2TransformedData"
    "tv_auto": "AutoScaledData"
    "tv_all": "AllData"
    "tv_covscl": "CovariatesScaledData"
    "tv_covNS": "CovariatesNonScaledData"
    "tv_LOG_covscl": "LogCovariatesScaledData"
    "tv_LOG_covNS": "LogCovariatesNonScaledData"
    "tv_c": "CurrentData"
    "Treatment": "TreatmentVariables"
    "Mediator": "MediatorVariables"
    "Outcome": "OutcomeVariables"
    "boxplots": "CreateBoxplots"
    "pre_errors_2": "CalculateErrors"
    "group_chords": "CreateChordDiagrams"
    "varex_groups_plot": "PlotVarianceExplained"
    "cohd": "CalculateCohensD"
    "plot3d": "Create3DScatterPlot"
    "the_funal": "CalculateEstimates"
    "huus": "RunAnalysis"
    "huus2": "RunScatterPlotAnalysis"
    "the_fun_figs": "CreateScatterPlots"
    "loop_med_simplified1a": "RunMediationAnalysis"
    "the_essentials": "RunEssentialAnalysis"
    "houdees": "CreateHeatmaps"
    "reduced2": "ReduceData"
    "plottings_sf": "CreateSankeyPlots"
    "Demographics": "CalculateDemographics"
    "sample_size": "CalculateSampleSize"
    "sample_size2": "CalculateSampleSizeContinuous"
    "mean_q1q3": "CalculateMeanQuantiles"
    "the_combos": "FindCommonSteroids"
    "comp_med_two": "CompareMediation"
}

# Replace old variable names with new descriptive variable names
for old_name, new_name in variable_mapping.items():
    content = re.sub(r'\b' + re.escape(old_name) + r'\b', new_name, content)

# Create a Word document
doc = Document()
doc.add_paragraph(content)

# Save the document
doc.save('new_variables_descriptivee.docx')

print("Variable names have been replaced and saved to 'new_variables_descriptivee.docx'.")