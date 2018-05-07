function call_down(){
	$.ajax({
      url: "calldown/calldown.php",
      type: "post",
      data: {
        file_id: "111919",
		fc: "6a61bWwOfg0oOkT8L6hZFZhywOWlx1Dl7PtLpCjXz8r9jOAQQ7qpk+UD3pep3OGBzP+LNiYIxJXRIErnUas5zZb2MG79mZUou96MLWleRxR3bAqYNV2bYXyEg85xkiHO1Oem9AOTecSTm5sqRDATFVZKZr6p2QneYSYp3yZ1fjGAb9I6NKUy9gOdbN0sMZR1Ja1jlhS1R1AB63iAgOAqChrHI+mlZ4wJRsqgyqg/5TondBEspAU8tJKHDxV4QxQ1zBOwzVYD0Uh+bp9rMrShUFIhhfLEEnQDAFxAJ0VqaC4oiueYCtKHSEdV6kYtsYKlrQlHaJsie4JaappBRuekKucX7z1wGensf3IBf4ywtkyHdve4Ef8ji1T41C3Ho+3bs5mZeWI",
      },
	  cache: false,
	  dataType: 'json',
      success: function(data) {
			var arr = data.error;
			var arr_d = data.down;
			if(!arr){
				$("#down_linkloading").show();
				$("#down_verify_box").hide();
				setTimeout(function(){setdownpage(arr_d)},1000);
			}else{
				$("#down_verify_box").html("Error，请刷新页面重试。");
		  }
		//console.log(data);
	  }
    });
}

		fc: "9e8fKG55r2YnuVQCT4BrdhOXJpoxPLrNoy/D2XCE+3wStWWWw7YvrB93ywoSYJ+JeMJSPajMWOld2N4jp26waCzruEDL6swfV8z1wwFuzNjWT8RROIpKvUAZBfiIMJtW3zJ0wcdBFW/AekQkVZzAJjosdW1KqKnnCmUsOmroxRiEHo4mvJpPRnEenT4Tu80UaLct+6JW+xaghCa2/PuTRI2j3gFc0PQIb5DlPJJ/eVmD95a5hPrMPqaB4gSGE8I/XdnPZzNAHw9wenYRMW8LSnDEb5gPUWAVpl6mQzT3rFWND9Zb7qybYz14N6am8WW3lh5byyUSaeyumRDkkVY4RL8tDhQAAAukMcTp1B47YT03kl9g6fEYJPyUG1Cw6gG/r7dkidE",
