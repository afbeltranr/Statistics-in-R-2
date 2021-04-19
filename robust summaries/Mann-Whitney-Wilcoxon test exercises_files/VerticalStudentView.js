!function(e,t){for(var s in t)e[s]=t[s]}(window,webpackJsonp([12,31],{"./common/lib/xmodule/xmodule/assets/vertical/public/js/vertical_student_view.js":function(e,t,s){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),function(e){var t=s("./openedx/features/course_bookmarks/static/course_bookmarks/js/views/bookmark_button.js"),i=s.n(t),n=s("./lms/static/completion/js/CompletionOnViewService.js");new Set;window.VerticalStudentView=function(t,o){var a=e(o),r=a.find(".bookmark-button");return s.i(n.markBlocksCompletedOnViewIfNeeded)(t,o),new i.a({el:r,bookmarkId:r.data("bookmarkId"),usageId:a.data("usageId"),bookmarked:a.parent("#seq_content").data("bookmarked"),apiUrl:r.data("bookmarksApiUrl")})}}.call(t,s(0))},"./lms/static/completion/js/CompletionOnViewService.js":function(e,t,s){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),function(e){function i(t,s){var i=e(s).find(".xblock-student_view[data-mark-completed-on-view-after-delay]").get();if(i.length>0){var a=new n.a;i.forEach(function(e){var t=parseInt(e.dataset.markCompletedOnViewAfterDelay,10);t>=0&&a.addElement(e,t)}),a.addHandler(function(s,i){var n=s.dataset.usageId;n&&!o.has(n)&&i.elementHasBeenViewed&&e.ajax({type:"POST",url:t.handlerUrl(s,"publish_completion"),data:JSON.stringify({completion:1})}).then(function(){o.add(n),s.dataset.markCompletedOnViewAfterDelay=0})})}}t.markBlocksCompletedOnViewIfNeeded=i;var n=s("./lms/static/completion/js/ViewedEvent.js"),o=new Set}.call(t,s(0))},"./lms/static/completion/js/ViewedEvent.js":function(e,t,s){"use strict";function i(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}function n(e,t){function s(){var s=Date.now();i+t<s&&(i=s,e())}var i=0;return s}s.d(t,"a",function(){return r});var o=function(){function e(e,t){for(var s=0;s<t.length;s++){var i=t[s];i.enumerable=i.enumerable||!1,i.configurable=!0,"value"in i&&(i.writable=!0),Object.defineProperty(e,i.key,i)}}return function(t,s,i){return s&&e(t.prototype,s),i&&e(t,i),t}}(),a=function(){function e(t,s,n){i(this,e),this.el=t,this.viewedAfterMs=s,this.callback=n,this.topSeen=!1,this.bottomSeen=!1,this.seenForMs=0,this.becameVisibleAt=void 0,this.hasBeenViewed=!1}return o(e,[{key:"getBoundingRect",value:function(){return this.el.getBoundingClientRect()}},{key:"handleVisible",value:function(){var e=this;this.becameVisibleAt||(this.becameVisibleAt=Date.now(),setTimeout(function(){e.checkIfViewed()},this.viewedAfterMs-this.seenForMs))}},{key:"handleNotVisible",value:function(){this.becameVisibleAt&&(this.seenForMs=Date.now()-this.becameVisibleAt),this.becameVisibleAt=void 0}},{key:"markTopSeen",value:function(){this.topSeen=!0,this.checkIfViewed()}},{key:"markBottomSeen",value:function(){this.bottomSeen=!0,this.checkIfViewed()}},{key:"getTotalTimeSeen",value:function(){return this.becameVisibleAt?this.seenForMs+(Date.now()-this.becameVisibleAt):this.seenForMs}},{key:"areViewedCriteriaMet",value:function(){return this.topSeen&&this.bottomSeen&&this.getTotalTimeSeen()>=this.viewedAfterMs}},{key:"checkIfViewed",value:function(){this.hasBeenViewed||this.areViewedCriteriaMet()&&(this.hasBeenViewed=!0,this.callback(this.el,{elementHasBeenViewed:this.hasBeenViewed}))}}]),e}(),r=function(){function e(){i(this,e),this.elementViewings=new Set,this.handlers=[],this.registerDomHandlers()}return o(e,[{key:"addElement",value:function(e,t){var s=this;this.elementViewings.add(new a(e,t,function(e,t){return s.callHandlers(e,t)})),this.updateVisible()}},{key:"addHandler",value:function(e){this.handlers.push(e)}},{key:"updateVisible",value:function(){this.elementViewings.forEach(function(e){if(!e.hasBeenViewed){var t=Date.now(),s=e.getBoundingRect(),i=!1;s.top>0&&s.top<window.innerHeight&&(e.markTopSeen(t),i=!0),s.bottom>0&&s.bottom<window.innerHeight&&(e.markBottomSeen(t),i=!0),s.top<0&&s.bottom>window.innerHeight&&(i=!0),i?e.handleVisible(t):e.handleNotVisible(t)}})}},{key:"registerDomHandlers",value:function(){var e=this;window.onscroll=n(function(){return e.updateVisible()},100),window.onresize=n(function(){return e.updateVisible()},100),this.updateVisible()}},{key:"callHandlers",value:function(e,t){this.handlers.forEach(function(s){s(e,t)})}}]),e}()},"./lms/static/js/views/message_banner.js":function(e,t,s){"use strict";var i,n;i=[s(3),s(0),s(1),s(2),s("./lms/templates/fields/message_banner.underscore"),s("./node_modules/edx-ui-toolkit/src/js/utils/html-utils.js")],void 0!==(n=function(e,t,s,i,n,o){return i.View.extend({events:{"click #close":"closeBanner"},closeBanner:function(e){sessionStorage.setItem("isBannerClosed",!0),this.hideMessage()},initialize:function(e){s.isUndefined(e)&&(e={}),this.options=s.defaults(e,{urgency:"high",type:"",hideCloseBtn:!0,isRecoveryEmailMsg:!1})},render:function(){return s.isUndefined(this.message)||s.isNull(this.message)?this.$el.html(""):this.$el.html(s.template(n)(s.extend(this.options,{message:this.message,HtmlUtils:o}))),this},showMessage:function(e){this.message=e,null==sessionStorage.getItem("isBannerClosed")&&this.render()},hideMessage:function(){this.message=null,this.render()}})}.apply(t,i))&&(e.exports=n)},"./lms/templates/fields/message_banner.underscore":function(e,t){e.exports='<div id="banner-msg" class="wrapper-msg urgency-<%- urgency %> <%- type %> <% if (isRecoveryEmailMsg == true) { %> recovery-email-alert <% } %>" role="alert">\n  <i <% if (hideCloseBtn == true) { %> hidden <% } %> id="close" class="fa fa-close close-icon"></i>\n    <div class="msg">\n        <div class="msg-content">\n            <div class="copy">\n                <p><%= HtmlUtils.HTML(message) %></p>\n            </div>\n        </div>\n    </div>\n</div>\n'},"./openedx/features/course_bookmarks/static/course_bookmarks/js/views/bookmark_button.js":function(e,t,s){"use strict";var i,n;i=[s(3),s(0),s(1),s(2),s("./lms/static/js/views/message_banner.js")],void 0!==(n=function(e,t,s,i,n){return i.View.extend({errorMessage:e("An error has occurred. Please try again."),bookmarkText:e("Bookmark this page"),bookmarkedText:e("Bookmarked"),events:{click:"toggleBookmark"},showBannerInterval:5e3,initialize:function(e){this.apiUrl=e.apiUrl,this.bookmarkId=e.bookmarkId,this.bookmarked=e.bookmarked,this.usageId=e.usageId,this.setBookmarkState(this.bookmarked)},toggleBookmark:function(e){e.preventDefault(),this.$el.prop("disabled",!0),this.$el.hasClass("bookmarked")?this.removeBookmark():this.addBookmark()},addBookmark:function(){var e=this;t.ajax({data:{usage_id:e.usageId},type:"POST",url:e.apiUrl,dataType:"json",success:function(){e.$el.trigger("bookmark:add"),e.setBookmarkState(!0)},error:function(t){var s,i;try{s=t.responseText?JSON.parse(t.responseText):"",i=s?s.user_message:"",e.showError(i)}catch(t){e.showError()}},complete:function(){e.$el.prop("disabled",!1),e.$el.focus()}})},removeBookmark:function(){var e=this,s=e.apiUrl+e.bookmarkId+"/";t.ajax({type:"DELETE",url:s,success:function(){e.$el.trigger("bookmark:remove"),e.setBookmarkState(!1)},error:function(){e.showError()},complete:function(){e.$el.prop("disabled",!1),e.$el.focus()}})},setBookmarkState:function(e){e?(this.$el.addClass("bookmarked"),this.$el.attr("aria-pressed","true"),this.$el.find(".bookmark-text").text(this.bookmarkedText)):(this.$el.removeClass("bookmarked"),this.$el.attr("aria-pressed","false"),this.$el.find(".bookmark-text").text(this.bookmarkText))},showError:function(e){var i=e||this.errorMessage;this.messageView||(this.messageView=new n({el:t(".message-banner"),type:"error"})),this.messageView.showMessage(i),setTimeout(s.bind(function(){this.messageView.hideMessage()},this),this.showBannerInterval)}})}.apply(t,i))&&(e.exports=n)}},["./common/lib/xmodule/xmodule/assets/vertical/public/js/vertical_student_view.js"]));