// A few jQuery helpers for exporting only
jQuery.fn.pop = [].pop;
jQuery.fn.shift = [].shift;

var InteractiveTable = {

    new: function (tableDiv) {

        var $TABLE = tableDiv;

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
            var $clone = $TABLE.find('tr.hide').clone(true).removeClass('hide table-line');
            $TABLE.find('table').append($clone);
            setEmptyRowVisible( false );
        });

        $TABLE.find('.table-remove').click(function () {
            $(this).parents('tr').detach();
            if ( isTableEmpty() ) {
                setEmptyRowVisible(true);
            }
        });

        $TABLE.find('.table-up').click(function () {
            var $row = $(this).parents('tr');
            if ($row.index() === 1) return; // Don't go above the header
            $row.prev().before($row.get(0));
        });

        $TABLE.find('.table-down').click(function () {
            var $row = $(this).parents('tr');
            $row.next().after($row.get(0));
        });

        var clearFn = function () {
            var $rows = dataRows();
            $rows.each(function () {
                $(this).detach();
            });
            setEmptyRowVisible(true);
        };

        return {
            export: function () {
                var $rows = $TABLE.find("tr:visible").not("tr.empty");
                var data = [];

                // Turn all existing rows into a loopable array
                $rows.each(function (i) {
                    var $td = $(this).find('td');
                    /*var h = {};

                    // Use the headers from earlier to name our hash keys
                    headers.forEach(function (header, i) {
                        h[header] = $td.eq(i).text();
                    });
					*/
					var h = [];
					$td.each( function(i) {
						
						h.push($(this).text());
					});
                    data.push(h);
                });
				data.shift();
                return data;
            },

            clear: clearFn,

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
