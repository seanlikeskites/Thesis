SELECT AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Tristimulus_2 END) AS H2_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Tristimulus_2 END) AS H2_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Peak_Tristimulus_2 END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Peak_Tristimulus_2 END)) AS H2_Diff,

       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Tristimulus_2 END) AS PH2_Unproc,
       AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Tristimulus_2 END) AS PH2_Proc,
       SIGN(AVG(CASE WHEN a.SignalState = 'processed' THEN a.Harmonic_Tristimulus_2 END) - 
       AVG(CASE WHEN a.SignalState = 'unprocessed' THEN a.Harmonic_Tristimulus_2 END)) AS PH2_Diff

FROM SAFEEqualiserAudioFeatureData AS a
INNER JOIN
(
	SELECT ID
	FROM SAFEEqualiserUserData AS b
	WHERE Descriptors = 'warm'
) as c
ON a.ID = c.ID
GROUP BY a.ID;
