if (typeof jQuery === 'undefined') {
  throw new Error('Bootstrap\'s JavaScript requires jQuery')
}
else
{
  $(document).ready(function()
  {
    $('a[href*=#]').click(function() 
    {
      if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'')
      && location.hostname == this.hostname) 
      {
        var $target = $(this.hash);
        $target = $target.length && $target
        || $('[name=' + this.hash.slice(1) +']');
        if ($target.length) 
        {   
          if( $('.header').hasClass('fixed') )
          {
            var targetOffset = $target.offset().top - $('.header').height()  - 20;  
          }
          else
          {
            var targetOffset = $target.offset().top - 90 - $('.header').height();
          }
          
          $('html,body')
          .animate({scrollTop: targetOffset}, 1000);
          return false;
        }
      }
    });

    var lastScrollTop = 0;

    $(window).bind('scroll', function()
    {
      var barra = $(window).scrollTop();
      var posicion = barra * 0.5;
      var opacity = 0;

      if (barra < lastScrollTop)
      {
        opacity = parseFloat ( $('.jumbotron').css('opacity') ) + 0.2;
      } else {
        opacity = parseFloat( $('.jumbotron').css('opacity') ) - 0.2;
      }

      $('.jumbotron').css({'opacity': opacity});

      lastScrollTop = barra;
      
      $('body').css({
        'background-position': 'center -' + ( posicion ) + 'px'
      });

      if ($(window).scrollTop() > 50) {
             $('.header').addClass('fixed');
             $('.pn-top').fadeIn();
      }
      else {
         $('.header').removeClass('fixed');
         $('.pn-top').fadeOut();
      }      
    });

    /* Show read more for seminars */
    $('.pn-readmore-introtext').click(function()
    {
      $this = $(this);

      $this.parent().find('.pn-readmore-fulltext').animate({
        height: "toggle"
      }, 1000 );
    });    
  });
}

