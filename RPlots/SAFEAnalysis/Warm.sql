SELECT AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Roll_Off END) AS RO_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Roll_Off END) AS RO_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Roll_Off END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Roll_Off END)) AS RO_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Centroid END) AS Centr_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Centroid END) AS Centr_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Centroid END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Centroid END)) AS Centr_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Centroid END) AS PCentr_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Centroid END) AS PCentr_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Centroid END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Centroid END)) AS PCentr_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Centroid END) AS HCentr_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Centroid END) AS HCentr_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Centroid END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Centroid END)) AS HCentr_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Variance END) AS Var_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Variance END) AS Var_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Variance END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Variance END)) AS Var_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Variance END) AS PVar_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Variance END) AS PVar_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Variance END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Variance END)) AS PVar_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Variance END) AS HVar_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Variance END) AS HVar_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Variance END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Variance END)) AS HVar_Diff

FROM SAFEDistortionAudioFeatureData AS a
INNER JOIN
(
	SELECT ID
	FROM SAFEDistortionUserData AS b
	WHERE Descriptors = 'warm'
) as c
ON a.ID = c.ID
GROUP BY a.ID;
