module MAG.Server.UI

open MAG
open MAG.Commands
open MAG.Server
open System.Runtime.Caching
open Chessie.ErrorHandling
open Chiron

let cache = new MemoryCache("UI Cache")

let inline unjson x = x |> Json.parse |> Json.deserialize
let inline mkjson x = x |> Json.serialize |> Json.format

let inline private get streamName foldName fold startState =
    async {
        let key = streamName + "||" + foldName
        match cache.Get key with
        | null ->
            let! newState = EventStore.refresh startState 0 fold streamName
            cache.Set(key, newState, CacheItemPolicy())
            return newState
        | item ->
            let cachedState, lastEvent = unbox item
            let! newState = EventStore.refresh cachedState (lastEvent + 1) fold streamName
            cache.Set(key, newState, CacheItemPolicy())
            return newState
    }

let getGame gid =
    get (sprintf "game-%s" gid) "gameFold" Fold.gameFold (ok Nothing)

let getPlayerView player gid =
    get (sprintf "game-%s" gid) (sprintf "playerViewFold||%A" player) (Fold.playerViewFold player) PlayerView.Nothing

let postStartGame players =
    let playerConfigs = unjson players
    let gid = System.Guid.NewGuid()
    let events =
        GameCommands.processCommand
            Nothing
            (StartGame (GameId gid, playerConfigs))    
    match events with
    | Ok (es, _) ->
        EventStore.persist -1 "gameEvent" es (sprintf "game-%A" gid)
        |> Async.map (fun _ -> mkjson gid)
    | Result.Fail error -> failwith <| sprintf "%A" error

let postPlayInitiative gid player cardStr =
    async {
        let card = unjson cardStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (PlayInitiative (PlayerName player, card)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let postPlayAttack gid player attackStr =
    async {
        let target, cards = unjson attackStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (PlayAttack (PlayerName player, target, cards)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let postCounter gid player cardsStr =
    async {
        let cards = unjson cardsStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (Counter (PlayerName player, cards)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let postMoveToStance gid player cardStr =
    async {
        let card = unjson cardStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (MoveToStance (PlayerName player, card)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let html player gid = 
    sprintf """<!DOCTYPE html>
<html>
    <head>
        <script src="/react.js"></script>
        <script src="/JSXTransformer.js"></script>
        <script src="/jquery.js"></script>
        <link rel="stylesheet" href="/bootstrap.css">
        <link rel="stylesheet" href="/bootstrap-theme.css">
        <script src="/bootstrap.js"></script> 
    </head>
    <body>
        <div id="main" class="container-fluid"></div>
        <script type="text/jsx">
            var player = '%s';
            var gid = '%s';

            var Card = React.createClass({
                render: function () {
                    if (this.props.card.type === 'basic') {
                        return (
                                <li>{ this.props.card.suit }-{ this.props.card.value }</li>
                        );
                    } else if (this.props.card.type === 'knockdown') {
                        return (<li>Knockdown!</li>);
                    } else if (this.props.card.type === 'mega') {
                        return (<li>{ this.props.card.name } ({ this.props.card.speed }/{ this.props.card.damage })</li>);
                    } else if (this.props.card.type === 'combo') {
                        return (<li>Combo! ({ this.props.card.speed }/{ this.props.card.extra })</li>);
                    }
                }
            });

            var CardList = React.createClass({
                render: function () {
                    var cards = this.props.cards.map(
                                function (card, i) { return (<Card card={card} key={i} />); }                    
                            );
                    return (<ul>{ cards }</ul>);
                }
            });

            var You = React.createClass({
                render: function() {
                    return (
                        <div>
                            <h3>You are: { this.props.you.name } - Life { this.props.you.life }</h3>
                            <h3>Hand:</h3>
                            <CardList cards={ this.props.you.hand } />
                            <h3>Stance:</h3>
                            <CardList cards={ this.props.you.stance } />
                            <h3>Discards:</h3>
                            <CardList cards={ this.props.you.discards } />
                        </div>);
                }
            });

            var Them = React.createClass({
                render: function () {
                    return (
                        <li>
                            <h3>They are: { this.props.them.name } - Life { this.props.them.life }</h3>
                            <h3>Stance:</h3>
                            <CardList cards={ this.props.them.stance } />
                            <h3>Discards:</h3>
                            <CardList cards={ this.props.them.discards } />
                        </li>);
                }
            });

            var ThemList = React.createClass({
                render: function () {
                    var them = this.props.them.map(
                                function (them, i) { return (<Them them={them} key={i} />); }                    
                            );
                    return (<ul>{ them }</ul>);
                }
            });

            var HandSelect = React.createClass({
                getInitialState: function() { return { value: null }; },
                handleChange: function(e) {
                    this.setState({ value: e.target.value });
                },
                render: function () {
                    var cards = this.props.cards.map(
                            function (card) { return (<option value="{ card }"><Card card={card} /></option>); }
                        );
                    return (
                        <select value={this.state.value}>
                            {cards}
                            <option value={null} onChange={ this.handleChange }>None</option>
                        </select>
                    );
                }
            });

            var StanceSelect = React.createClass({
                getInitialState: function() { return { value: [] }; },
                handleChange: function(e) {
                    console.log(e.target);
                    console.log(e.target.value);
                    if (this.state.value.indexOf(e.target.value)) {
                        this.state.value.push(e.target.value);
                        console.log(this.state.value);
                    } else {
                        this.setState({ value: [] });
                        console.log(this.state.value);
                    }
                },
                render: function () {
                    var cards = this.props.cards.map(
                            function (card, i) { 
                                return (<option key={ i } value={ i }><Card card={card} /></option>);
                            }
                        );
                    return (
                        <select multiple={true} value={this.state.value} onChange={ this.handleChange }>{cards}</select>
                    );
                }
            });

            var CommandBox = React.createClass({
                post: function (action, data) {
                    var baseUrl = '/api/player/' + player + '/' + gid + '/';
                    $.ajax({
                        url: baseUrl + action,
                        dataType: 'json',
                        type: 'POST',
                        data: data,
                        success: function (d) {
                            return;
                        },
                        error: function (xhr, status, error) {
                            console.error(baseUrl, status, error.toString());
                        }.bind(this)
                    });
                },
                handleSubmit: function (action, e) {
                    e.preventDefault();
                    if (action === 'initiative') {
                        var card = React.findDOMNode(this.refs.card).value.trim();
                        this.post(action, card);
                    } else if (action === 'stance') {
                        var card = React.findDOMNode(this.refs.card).value.trim();
                        this.post(action, card);
                    } else if (action === 'counter') {
                        console.log("other stuff");
                        var cards = React.findDOMNode(this.refs.cards).value.trim();
                        this.post(action, cards);
                    } else if (action === 'attack') {
                        var cards = React.findDOMNode(this.refs.cards).value.trim();
                        var target = React.findDOMNode(this.refs.target).value.trim();
                        this.post(action, [target, cards]);
                    }
                },
                render: function () {
                    if (this.props.waiting === 'You to play initiative') {
                        var action = 'initiative';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <HandSelect cards={this.props.hand} />
                                    <input type="submit" value="Post" />
                                </form>);
                    } else if (this.props.waiting === 'You to move a card to stance') {
                        var action = 'stance';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <HandSelect cards={this.props.hand} />
                                    <input type="submit" value="Post" />
                                </form>);
                    } else if (this.props.waiting === 'You to counter') {
                        var action = 'counter';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <HandSelect cards={this.props.hand} />
                                    <input type="submit" value="Post" />
                                </form>);
                    } else if (this.props.waiting === 'You to attack') {
                        var action = 'attack';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <input type="text" placeholder="A target" ref="playerName" />
                                    <HandSelect cards={this.props.hand} />
                                    <StanceSelect cards={this.props.stance} />
                                    <input type="submit" value="Post" />
                                </form>);
                    }
                    return (<div />);
                }
            });

            var GameView = React.createClass({
                getInitialState: function () {
                    return { download: [
                        {
                            data: {
                                you: {
                                    hand: [],
                                    name: "",
                                    stance: [],
                                    life: 0,
                                    discards: []
                                },
                                them: [],
                                waitingFor: { waiting: "" }
                            }
                        }, 0]};
                },
                loadData: function() {
                    $.ajax({
                        url: '/api/player/' + player + '/' + gid,
                        dataType: 'json',
                        success: function (data) {
                            this.setState({ download : data });
                        }.bind(this),
                        error: function (xhr, status, err) {
                            console.error("oh noes", status, err.toString());
                        }.bind(this)
                    });
                },
                componentDidMount: function () {
                    this.loadData();
                    setInterval(this.loadData, 1000);
                },
                render: function () {
                    var waiting = this.state.download[0].data.waitingFor.waiting;
                    var you = this.state.download[0].data.you;
                    return (
                        <div>
                            <h3>The game is waiting for: { waiting } during { this.state.download[0].data.turn }'s turn</h3>
                            <You you={ you } />
                            <ThemList them={ this.state.download[0].data.them } />
                            <CommandBox waiting={ waiting } hand={ you.hand } stance={ you.stance } />
                        </div>);
                }
            });

            React.render(
                <GameView />,
                document.getElementById('main')
            );
        </script>
    </body>
</html>""" player gid