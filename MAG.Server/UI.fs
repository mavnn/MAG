module MAG.Server.UI

let html name = 
    sprintf
        """<!DOCTYPE html>
<html>
    <head>
        <script src="http://fb.me/react-0.13.1.js"></script>
        <script src="http://fb.me/JSXTransformer-0.13.1.js"></script>
    </head>
    <body>
        <div id="main"></div>
        <script type="text/jsx">
            React.render(
                <h1>hello %s</h1>,
                document.getElementById('main')
            );
        </script>
    </body>
</html>
""" <| name