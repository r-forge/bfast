
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->


BFAST:

 

<ul> 

<li> BFAST iteratively estimates the time and number of abrupt changes within time series, and characterizes change by its magnitude and direction. BFAST can be used to analyze different types of time series and can be applied to other disciplines dealing with seasonal or non-seasonal time series, such as hydrology, climatology, and econometrics. The algorithm can be extended to label detected changes with information on the parameters of the fitted piecewise linear models.</li> 

<li> Verbesselt, J., Hyndman, R., Newnham, G., & Culvenor, D. (2010). Detecting trend and seasonal changes in satellite image time series. Remote Sensing of Environment, 114, 106-115. <a href="http://dx.doi.org/10.1016/j.rse.2009.08.014">DOI: 10.1016/j.rse.2009.08.014</a>. <a href="http://bfast.r-forge.r-project.org/RSE_ChangeDetection_InPress_JanVerbesselt.pdf">DownLoad Paper</a></li>
<li> Verbesselt, J., Hyndman, R., Zeileis, A., & Culvenor, D. (2010). Phenological change detection while accounting for abrupt and gradual trends in satellite image time series. Remote Sensing of Environment, 114, 2970-2980. <a href="http://dx.doi.org/10.1016/j.rse.2010.08.003">DOI: 10.1016/j.rse.2010.08.003</a>. <a href="http://bfast.r-forge.r-project.org/Verbesselt+Hyndman+Zeileis-2010.pdf">DownLoad Paper</a></li> 

</ul>

<img src="http://bfast.r-forge.r-project.org/seasonalbreak_TreeMort.jpg" width="400" height="300" align=MIDDLE

alt="Fitted seasonal, trend and remainder (i.e. estimated noise) components for a 16-day MODIS NDVI time series (data series) of a pine plantation in the

northern part of the study area. Three abrupt changes are detected in the trend component of the time series. Time (- - -), corresponding conidence interval

(red), direction and magnitude of abrupt change and slope of the gradual change are shown in the estimated trend component. The solid bars on the right hand side of the plot show the same data range, to aid comparisons." longdesc="Detecting changes in seasonal time series">



<p> Figure illustrating the fitted seasonal, trend and remainder (i.e. estimated noise) components for a 16-day MODIS NDVI time series (data series) of a pine plantation in the

northern part of the study area. Three abrupt changes are detected in the trend component of the time series. Time (- - -), corresponding conidence interval

(red), direction and magnitude of abrupt change and slope of the gradual change are shown in the estimated trend component. The solid bars on the right hand side of the plot show the same data range, to aid comparisons. See paper for more details about the data and analysis. </p>



<!-- <p> The temporary windows builds can be dowloaded <a href="http://bfast.r-forge.r-project.org/bfast_1.1.zip"><strong>here</strong></a>.</p> -->

<p> <strong> Applications: </strong> Deforestation, forest health monitoring and phenological change detection within time series of spatio-temporal data sets (satellite images).
</p>

<p> Contact: Jan.Verbesselt'@'wur.nl. The current BFAST package version is 1.3 on R-Forge and 1.2-1 on R CRAN  (Apr 15, 2011). To install the most recent version (bug fixes) directly within R type: <strong> install.packages("bfast", repos="http://R-Forge.R-project.org") </strong> </p>


<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
