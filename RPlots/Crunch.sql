SELECT AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Irregularity_K END) AS Ik_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Irregularity_K END) AS Ik_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Irregularity_K END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Irregularity_K END)) AS Ik_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Irregularity_K END) AS PIk_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Irregularity_K END) AS PIk_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Irregularity_K END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Irregularity_K END)) AS PIk_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Irregularity_K END) AS HIk_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Irregularity_K END) AS HIk_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Irregularity_K END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Irregularity_K END)) AS HIk_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Skewness END) AS Skew_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Skewness END) AS Skew_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Spectral_Skewness END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Spectral_Skewness END)) AS Skew_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Skewness END) AS PSkew_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Skewness END) AS PSkew_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Skewness END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Skewness END)) AS PSkew_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Skewness END) AS HSkew_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Skewness END) AS HSkew_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Skewness END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Skewness END)) AS HSkew_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Kurtosis END) AS PKurt_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Kurtosis END) AS PKurt_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Spectral_Kurtosis END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Spectral_Kurtosis END)) AS PKurt_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Kurtosis END) AS HKurt_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Kurtosis END) AS HKurt_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Spectral_Kurtosis END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Spectral_Kurtosis END)) AS HKurt_Diff

FROM SAFEDistortionAudioFeatureData AS a
INNER JOIN
(
	SELECT ID
	FROM SAFEDistortionUserData AS b
	WHERE Descriptors = 'crunch' OR Descriptors = 'crunchy'
) as c
ON a.ID = c.ID
GROUP BY a.ID;

