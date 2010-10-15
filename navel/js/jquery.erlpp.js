//
//
//

(function($) {

	$.erlpp = function(what, options) {
		var opts = $.extend({}, $.erlpp.defaults, options);
		return format(what, opts);		
	};
	
	$.erlpp.defaults = {};

	function format(t, opts) {	
		if (typeof(t) == 'number') {
			return t.toString();	// small integer or float
		}
		else if (typeof(t) == 'string')	{
			return t;	// atom
		}
		else if (typeof(t) == 'object' && typeof(t.length) != 'undefined') {
			var es = $.map(t, function(e) {
				if (typeof(e) == 'undefined')
					console.log(t);
				return format(e, opts);
			});
			return '['+es.join(',')+']';
		}
		else if (typeof(t) == 'object') {
			var selector;
			for(var key in t) {
				if(t.hasOwnProperty(key)) {
					selector = key;
					break;
				}
			}
			
			switch (selector) {
			case 'n':	// bignum
				return t.n;
			case 'b':	// binary
				if (t.b.length > 64)
					return '#binary';
				else
				{
					var bytes = [];
					for (var i = 0; i < t.b.length; i += 2)
						bytes.push(parseInt(t.b.substr(i, 2), 16));
					return '<<' + bytes.join(',') + '>>';
				}
			case 'f':	// fun
				return '#fun';
			case 'o':	// oid
				return '#oid';
			case 'p':	// pid
				return '#pid';
			case 't':	// tuple
				var es = $.map(t.t, function(e) {
					return format(e, opts);
				});
				return '{'+es.join(',')+'}';
			case 's':	// string
				return '"'+t['s']+'"';
			case 'x':	// partial
				return '#partial';
			default:
				console.log(selector);
				throw new Error();
			}
		}
		else {
			console.log(t);
			throw new Error();
		}
	}
	
})(jQuery);

//EOF
