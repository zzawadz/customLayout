(function() {
	window.counter = 'https://customlayout_zstat.goatcounter.com/count'

	var script = document.createElement('script');
	script.async = 1;
	script.src = '//gc.zgo.at/count.js';
	var ins = document.getElementsByTagName('script')[0];
	ins.parentNode.insertBefore(script, ins)
})();
