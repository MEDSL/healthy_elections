// Calculate Graves for each row in the table

function makeTable() {
	var data = table.export();
	var $tbody = $("#resultTable").find("tbody"); // get the table body
	$tbody.find("tr").detach(); // clear thte table body of <tr>s
	for ( index in data ) {
		var row = data[index];
		var lambda = row[6];
		var tau = row[5];
		var k = row[4];
		//var X = row[4];
		//var Y = row[5];
		var capacity = row[1];
		var staff = row[2];
		var stations = row[3];
		var maxTime = row[7];
		//var joinRate = row[8];
		//var Brate = 1 - joinRate;
		var Brate = 0;
		//results.push( {precinct:row["precinct #"], results:graves(lambda, tau, k, X, Y)} );
		//console.log(lambda, tau, k, capacity, staff, stations, Brate);
		var res = graves(lambda, parseFloat(tau), parseInt(k), parseInt(capacity), parseInt(staff), parseInt(stations), Brate, parseFloat(maxTime));
		$tbody.append( "<tr><th>" + row[0] + "</th><td>" 
						+ res.aveW + "</td><td>" 
						//+ res.aveSysT + "</td><td>" 
						//+ res.expectedInSystem + "</td><td>" 
						+ ((parseFloat(res.expectedQLenIn) + parseFloat(res.expectedQLenOut))).toFixed(1) + "</td><td>" 
						+ res.expectedQLenOut + "</td><td>" 
						//+ res.probNoWait + "</td><td>" 
						+ (parseFloat(res.probBlock)*100).toFixed(0) + "%</td><td>" 
						+ ((1-parseFloat(res.shortWaitProb))*100).toFixed(0) + "%</td><td class='cell-alert'>"
						//+ res.effArrRate + "</td><td class='cell-alert'>" 
//						+ res.reduceW + "</td><td class='cell-alert'>" 
						+ res.alertMessage + "</td></tr>");
	}
}