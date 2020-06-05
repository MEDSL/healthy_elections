// Calculate Graves for each row in the table

function makeTable() {
	var data = table.export();
	var $tbody = $("#resultTable").find("tbody"); // get the table body
	$tbody.find("tr").detach(); // clear thte table body of <tr>s
	for ( index in data ) {
		var row = data[index];
		var lambda = row[6];
		var tau = row[4];
		var k = row[5];
		//var X = row[4];
		//var Y = row[5];
		var capacity = row[1];
		var staff = row[2];
		var stations = row[3];
		var joinRate = row[7];
		var Brate = 1 - joinRate;
		//results.push( {precinct:row["precinct #"], results:graves(lambda, tau, k, X, Y)} );
		var res = graves(lambda, tau, parseInt(k), parseInt(capacity), parseInt(staff), parseInt(stations), Brate);
		$tbody.append( "<tr><th>" + row[0] + "</th><td>" 
						+ res.aveW + "</td><td>" 
						+ res.aveSysT + "</td><td>" 
						+ res.expectedInSystem + "</td><td>" 
						+ res.expectedQLenIn + "</td><td>" 
						+ res.expectedQLenOut + "</td><td>" 
						+ res.probNoWait + "</td><td>" 
						+ res.probBlock + "</td><td>" 
						+ res.effArrRate + "</td><td class='cell-alert'>" 
//						+ res.reduceW + "</td><td class='cell-alert'>" 
						+ res.alertMessage + "</td></tr>");
	}
}