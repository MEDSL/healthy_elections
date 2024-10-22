function graves( lambda, tau, k, capacity, staff, stations, Brate) {
	
	// var DataFrame = dfjs.DataFrame
	var Qmax = capacity-staff-stations;

	// NOTES ON THE UNITS OF INPUTS
	// lambda : voters per hour
	// tau : minutes per voter
	// k : number of machines
	// X : minutes
	// Y : percentage
	// Qmax : finite waiting room size
	// Brate : balking rate with full waiting room

	var validInputs = true;
	var stable = true;
	var alertmessage = "";

	// VALIDATE INPUTS RANGES
	if (lambda<1 || lambda>10000)	{
		alertmessage = "ERROR: Arrival rate not within range"
		validInputs = false;
	}
	if (tau<0 || tau>100)	{
		alertmessage = alertmessage+"\r"+ "ERROR: Average time to vote not within range."
		validInputs = false;
	}
	if (k<1 || k>100)	{
		alertmessage = alertmessage+"\r"+ "ERROR: Number of voting stations not within range."
		validInputs = false;
	}
	/*
	if (X<0 || X>60)	{
		alertmessage = alertmessage+"\r"+ "ERROR: Max wait time target not within range."
		validInputs = false;
	}
	if (Y<1 || Y>99.999)	{
		alertmessage = alertmessage+"\r"+ "ERROR: Service level not within range."
		validInputs = false;
	}
	*/
	if (Qmax<0)	{
			alertmessage = alertmessage+"\r"+ "ERROR: Maximum line length not within range. Ensure total voter stations plus election staff does not exceed building capacity."
			validInputs = false;
		} else if (Qmax>200) {
			alertmessage = alertmessage+"\r"+ "ERROR: Maximum line length not within range."
			validInputs = false;
		}
	if (Brate<0 || Brate>1)	{
		alertmessage = alertmessage+"\r"+ "ERROR: Outside Line Entry Chance not within range."
		validInputs = false;
	}
	if (k > stations) {
		alertmessage = alertmessage + "\r" + "ERROR: Number of bottleneck stations exceeds total number of stations"
		validInputs = false;
	} else if (k == stations) {
		alertmessage = alertmessage + "\r" + "Warning: Number of bottleneck stations is equal to total number of stations. Are there no other stations?"
	}
	
	// CHECK SYSTEM STABILITY
	//if (lambda >= k / tau *60)	{ 
	//	alertmessage = alertmessage+"\r"+ "ERROR: Data entered produces unstable results !\nOn average the number of people that arrive exceeds the number of people that the system can handle. To make the system stable, decrease the arrival rate or the time to vote, or increase the number of stations in the system."
	//	stable = false;
	//}	
	
	// CONVERT INPUTS
	var tau = tau / 60; 
	//var X = X / 60;
	//var Y = Y / 100;
	var numIter = Qmax * 10 + 1; //length of array to calculate discrete probability of each state
	
	// CLAIM OUTPUTS
	var aveW; // average waiting time (minutes)
	var aveSysT; //average system time (mins)
	//var reqW; // percent wait time greater than X (percentage)
	//var reqM; // number of machines required to meet service level
	var probBlock = 0; // probability that a voter will arrive to a full system
	var effArrRate = 0; // the effective entry rate of voters who choose to enter the system
	var expectedInSystem = 0; // expected number of people in the system
	var expectedQLenIn = 0; // expected queue length in the finite waiting room
	var expectedQLenOut = 0; // expected queue length outside (would only be non-zero with a full waiting room)


/*
	if (stable == true && validInputs == true) {	
		// 1. calculate average waiting time
		aveW = 60 * erlang(lambda, tau, k) / (k / tau - lambda);
		
		// 2. calculate the percent wait time greater than X
		reqW = 100 * erlang(lambda, tau, k) / Math.exp((k / tau - lambda) * X);

		// 3. calculate # of machines required to meet service level
		var c = Math.ceil(tau * lambda + 0.001);
		var e;
		do {
			e = erlang(lambda, tau, c) / Math.exp((c / tau - lambda) * X);
			if (e > 1 - Y) {
				c = c + 1; 
			}
			else {
				reqM = c;
				break;
			}
		} while(1);

		aveW = aveW.toFixed(1);
		reqW = reqW.toFixed(1);
		reqM = reqM.toFixed(0);

		return {
			aveW: aveW,
			reqW: reqW,
			reqM: reqM,
			probBlock: probBlock,
			effArrRate: effArrRate,
			alertMessage: alertmessage
		}
	}
*/
	
	if (validInputs == true) {
		//4. Create Data Frame for Waiting Room (calculate individually for each state of the world, within reasonable bound set by numIter)
		// Arrays that allow buildup
		var numInSys = Array.from(Array(numIter).keys()); // number of individuals in system
		var exponent = Array.from(Array(numIter).fill(0)); // buildup to relative weight
		var relativeWeight = Array.from(Array(numIter).fill(0)); // relative weight of each # in system
		var inSystemProbability = Array.from(Array(numIter).fill(0)); // normalized to a probability measure
		var inQueue = Array.from(Array(numIter).fill(0)); // number waiting in the finite waiting room for each # in system
		var outside = Array.from(Array(numIter).fill(0)); // number waiting outside the finite waiting room for each # in system
		var probQisZero = Array.from(Array(numIter).fill(0)); // probability that none are waiting
		var arrRate = Array.from(Array(numIter).fill(0)); // scaled arrival rate
		var queueFullFlag = Array.from(Array(numIter).fill(0)); // flag if waiting room is full
		// Integer number of max in system
		var maxInSystem = Qmax + k;

		// let exponent[0]=0 and set the e^(exponent[0])=1 manually
		relativeWeight[0] = 1;

		// loop through the exponent and relative weight vectors from index 1 to the end
		if (Brate == 1){
			for (i = 1; i < numIter; i++) {
				if (i <= maxInSystem){
					exponent[i] = exponent[i-1] + Math.log(lambda) - Math.log(1/tau) - Math.log(Math.min(numInSys[i],k));
					relativeWeight[i] = Math.exp(exponent[i]);
				} else {
					exponent[i] = exponent[i-1] - Math.log(1/tau) - Math.log(Math.min(numInSys[i],k));
					relativeWeight[i] = Math.exp(exponent[i]);
				}
			}
		} else {
			for (i = 1; i < numIter; i++) {
				if (i <= maxInSystem){
					//exponent[i] = i-1;
					exponent[i] = exponent[i-1] + Math.log(lambda) - Math.log(1/tau) - Math.log(Math.min(numInSys[i],k));
					relativeWeight[i] = Math.exp(exponent[i]);
				} else {
					//exponent[i] = i-1;
					exponent[i] = exponent[i-1] + Math.log(lambda) + Math.log(1-Brate) - Math.log(1/tau) - Math.log(Math.min(numInSys[i],k));
					relativeWeight[i] = Math.exp(exponent[i]);
				}
			}
		}

		// Total probability weight
		var totalProb = relativeWeight.reduce((a, b) => a + b, 0)

		for (i = 0; i < numIter; i++) {
			//calculate probability for each number of people in system
			inSystemProbability[i] = relativeWeight[i]/totalProb;
			//calculate the number of people in the queue if there are numInSys number in the system
			inQueue[i] = Math.min(Math.max(0,numInSys[i]-k),Qmax);
			//calculate the number of people outside
			outside[i] = Math.max(0,numInSys[i]-maxInSystem);
			//calculate probability Queue length is 0
			if (numInSys[i]<k){
				probQisZero[i] = inSystemProbability[i];
			} else {
				probQisZero[i] = 0;
			}
			// calculate arrival rate
			if (numInSys[i]<maxInSystem){
				arrRate[i] = lambda;
				queueFullFlag[i] = 0;
			} else {
				arrRate[i] = lambda*(1-Brate);
				queueFullFlag[i] = 1;
			}
			// cumulatively sum results. Result will be probability-weighted averages
			expectedInSystem += inSystemProbability[i] * numInSys[i];
			expectedQLenIn += inSystemProbability[i] * inQueue[i];
			expectedQLenOut += inSystemProbability[i] * outside[i];
			effArrRate += inSystemProbability[i] * arrRate[i];
			probBlock += inSystemProbability[i] * queueFullFlag[i];

		}

		//use littles law to calculate the rest of the results
		aveW = 60*(expectedQLenIn)/effArrRate;
		aveSysT = 60*(expectedInSystem)/effArrRate;

		// probability of no wait
		var probNoWait = probQisZero.reduce((a, b) => a + b, 0)
	
		// check stability
		if (effArrRate >= k / tau)	{ 
			alertmessage = alertmessage+"\r"+ "ERROR: Data entered produces unstable results !\nOn average the number of people that arrive exceeds the number of people that the system can handle. To make the system stable, decrease the arrival rate or the time to vote, or increase the number of stations in the system."
			stable = false;
		}	

		//Round answers
		aveW = aveW.toFixed(1);
		aveSysT = aveSysT.toFixed(1);
		probBlock = probBlock.toFixed(2);
		effArrRate = effArrRate.toFixed(1);
		expectedInSystem = expectedInSystem.toFixed(1);
		expectedQLenIn = expectedQLenIn.toFixed(1);
		expectedQLenOut = expectedQLenOut.toFixed(1);
		probNoWait = probNoWait.toFixed(2);

		return {
			aveW: aveW,
			//reqW: reqW,
			//reqM: reqM,
			aveSysT: aveSysT,
			probBlock: probBlock,
			effArrRate: effArrRate,
			expectedInSystem: expectedInSystem,
			expectedQLenIn: expectedQLenIn,
			expectedQLenOut: expectedQLenOut,
			probNoWait: probNoWait,
			alertMessage: alertmessage,
			Qmax: Qmax
		}
	}
	
	
//	results if unstable and/or invalid input produces alert - error message
	
	else {
			
		var aveW = "--";
		// var reqW = "--";
		// var reqM = "--";
		var aveSysT = "--";
		var probBlock = "--";
		var effArrRate = "--";
		var expectedInSystem = "--";
		var expectedQLenIn = "--";
		var expectedQLenOut = "--";
		
		return {		
			aveW: aveW,
			//reqW: reqW,
			//reqM: reqM,
			aveSysT: aveSysT,
			probBlock: probBlock,
			effArrRate: effArrRate,
			expectedInSystem: expectedInSystem,
			expectedQLenIn: expectedQLenIn,
			expectedQLenOut: expectedQLenOut,
			alertMessage: alertmessage,
			Qmax: Qmax
		}
	}
	
}

function erlang(lambda, tau, k) {
    var rho = lambda * tau / k;
    var tmp = 0;
    for (var i = 0; i <= k - 1; i++) {
		tmp += Math.pow((k * rho),i) / fac(i);
    }
    tmp += Math.pow((k * rho),k) / fac(k) / (1 - rho);
    var pi_0 = 1 / tmp;
    return Math.pow((k * rho),k) / fac(k) / (1 - rho) * pi_0;
}

function fac(n){
	if (n == 0) {
		return 1; 
	} else {
		return n * fac( n - 1 ); 
	}
}

function makeDataFrame() {

}

