// A few jQuery helpers for exporting only
jQuery.fn.pop = [].pop;
jQuery.fn.shift = [].shift;

var InteractiveTable = {

    new: function (tableDiv) {
        //Define Globals
        var $TABLE = tableDiv;
        var rowCounter = 1;
        //var inputs_validation = ["capacity", "staff", "totalstations", "stations", "time", "arrival", "target"];
        //var inputs_int = [true, true, true, true, false, false, false];
        //var inputs_positive = [true, false, true, true, true, true, false];

        var setEmptyRowVisible = function (shouldShow) {
            var emptyRow = $TABLE.find(".empty");
            if (shouldShow) emptyRow.show();
            else emptyRow.hide();
        };

        var dataRows = function() {
            return $TABLE.find("tbody tr:visible").not("tr.empty");
        };

        var isTableEmpty = function() {
          return dataRows().length===0;
        };

        $TABLE.find('.table-add').click(function () {
            // Create Row
            $TABLE.find("table tr:last").after('\
                <tr>\
                    <td><input type = "text" class = "form-control input-sm pollingplace_input" value ="'+rowCounter+'"/></td>\
                    <td><input class = "form-control input-sm qlen_input capacity_input" type="number"/></td>\
                    <td><input class = "form-control input-sm qlen_input staff_input" type="number"/></td>\
                    <td><input class = "form-control input-sm qlen_input totalstations_input" type="number"/></td>\
                    <<td class = "qcap align-top" >-</td>\
                    <td><input class = "form-control input-sm util_input stations_input" type="number"/></td>\
                    <td><input class = "form-control input-sm util_input time_input" type="number" step="0.25"/></td>\
                    <td><input class = "form-control input-sm util_input arrival_input" type="number"/></td>\
                    <td><input class = "form-control input-sm target_input" type="number" value = "30"/></td>\
                    <td class = "alerts"></td>\
                    <td class="controls">\
                        <button class="table-remove btn btn-danger btn-xs"><i class="fas fa-trash-alt"></i></button>\
                    </td>\
                </tr>\
            '); 
            

            //Remove Empty Row
            rowCounter++;
            setEmptyRowVisible( false );

            // Create funcitonality of delete row button
            $TABLE.find("table tr:last button")[0].addEventListener('click', function () {
                    console.log($(this).parents('tr'));
                    $(this).parents('tr').remove();
                    if ( isTableEmpty() ) {
                        setEmptyRowVisible(true);
                }
            });

            //Create Internal Line Capacity Functionality
            $TABLE.find("table tr:last")[0].getElementsByClassName("qlen_input").forEach(item => {
                item.addEventListener('blur', function(evt){
                    var inputs = item.closest('tr').getElementsByClassName("qlen_input");
                    var cap = inputs[0].value;
                    var staff = inputs[1].value;
                    var stations = inputs[2].value;
                    var qmax = cap-staff-stations;
                    item.closest('tr').getElementsByClassName("qcap")[0].innerHTML = qmax;
                    //handle click
                });
            });

            //Create Utilization Alerts
            $TABLE.find("table tr:last")[0].getElementsByClassName("util_input").forEach(item => {
                item.addEventListener('blur', function(evt){
                    var inputs = item.closest('tr').getElementsByClassName("util_input");
                    var k = inputs[0].value;
                    var tau = inputs[1].value;
                    var lambda = inputs[2].value;
                    var util = lambda/(k*60/tau);
                    if(util>=1){
                        item.closest('tr').getElementsByClassName("alerts")[0].innerHTML = '<button class="table-remove btn btn-danger btn-xs" \
                        data-toggle="tooltip" data-placement = "top" title="System over capacity. More stations or faster service needed."><i class="fas fa-exclamation"></i></button>';
                    } else if(util>(1/1.1)){
                        item.closest('tr').getElementsByClassName("alerts")[0].innerHTML = '<button class="table-remove btn btn-warning btn-xs" \
                        data-toggle="tooltip" data-placement = "top" title="System within 10% of capacity limit. Small changes in arrival rates will cause lines to grow."><i class="fas fa-exclamation"></i></button>';
                    } else {
                        item.closest('tr').getElementsByClassName("alerts")[0].innerHTML = "";
                    }
                    $(function () {
                        $('[data-toggle="tooltip"]').tooltip();
                    });
                    //handle click
                });
            });

            
        });

        //$TABLE.find('.table-remove').click(function () {
        //    console.log($(this).parents('tr'));
        //    $(this).parents('tr').remove();
        //    if ( isTableEmpty() ) {
        //        setEmptyRowVisible(true);
        //    }
        //});

        $TABLE.find('.table-up').click(function () {
            var $row = $(this).parents('tr');
            if ($row.index() === 1) return; // Don't go above the header
            $row.prev().before($row.get(0));
        });

        $TABLE.find('.table-down').click(function () {
            var $row = $(this).parents('tr');
            $row.next().after($row.get(0));
        });


        var updateMath = function () {
            var $rows = dataRows();
            $rows.each(function () {
                var cap = $(this).find('td.roomcap').text();
                var staff = $(this).find('td.staff').text();
                var stations = $(this).find('td.voting').text();
                var qlen = cap-staff-stations;
                var $td = $(this).find('td.qcap');
                $td.text(qlen);
            });
        };

        var clearFn = function () {
            var $rows = dataRows();
            $rows.each(function () {
                $(this).detach();
            });
            setEmptyRowVisible(true);
        };

        return {
            export: function () {
                //Find all rows in table body
                var $rows = $TABLE.find("tbody tr").not("tbody tr.empty");
                //console.log($rows);
                var data = [];

                // Turn all existing rows into a loopable array
                $rows.each(function (i) {
                    var $td = $(this).find('input');
                    //console.log($td)
					var h = [];
                    //Push input values to array
					$td.each( function(i) {
						h.push($(this)[0].value);
                        //console.log($(this)[0].value);
					});
                    //push to master array
                    data.push(h);
                });
                return data;
            },

            clear: clearFn,
            update: updateMath,

            // Sets the data in the table. Expects an array of objects, similar to those
            // obtained from export()
            setData: function (dataArr) {
                // map field name to column index
                var headers = [];
                var $rows = $TABLE.find('thead tr');
                $($rows.shift()).find('th:not(:empty)').each(function (i) {
                    headers.push($(this).text().toLowerCase());
                });

                // clear the table
                clearFn();

                // fill the rows
                for (var i in dataArr) {
                    var datum = dataArr[i];
                    var $clone = $TABLE.find('tr.hide').clone(true).removeClass('hide table-line');
                    for (var hIdx in headers) {
                        $clone.find("td").eq(hIdx).text(datum[headers[hIdx]]);
                    }
                    $TABLE.find('table').append($clone);
                }
                setEmptyRowVisible( dataArr.length === 0 );
                return headers;
            }
        };
    }

};
