(function () {

    $(document).ready(function () {
        $('.internal').hide();
        $('#toggle-internals').change(function () {
            if (this.checked) {
                $('.internal').show();
            } else {
                $('.internal').hide();
            }
        });
    });

})();