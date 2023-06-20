-module(client).
-import(server, [start/2]).
-export([registerUser/3, simulate/3]).

registerUser(Uid, ProcessId, NumUsers) ->
    ProcessId ! {makeOnline, Uid, self()},
    assignFollowersUsingZipF(Uid, ProcessId, NumUsers).

assignFollowersUsingZipF(CurrUid, ProcessId, UserCount) ->
    Divisor = CurrUid + 1,
    SubscriberCount = UserCount div Divisor,
    io:format("Subscriber list for UID ~p : ~p ~n", [CurrUid, SubscriberCount]),
    List = [rand:uniform(UserCount) || _ <- lists:seq(1, SubscriberCount)],
    List2 = checkIfUserIsInList(List, CurrUid),
    OrderedSet = ordsets:from_list(List2),
    FollowerList = ordsets:to_list(OrderedSet),
    ProcessId ! {setFollowers, CurrUid, FollowerList}.

simulate(UserList, ProcessId, StartIdx) ->
    Operations = [
        "Tweet",
        "Sign_Out",
        "Sign_In",
        "Subscribe",
        "Get_Hashtags",
        "Get_Mentions",
        "Get_Feed",
        "Repost"
    ],
    HashTags = ["HashTagOne", "HashTagTwo", "HashTagThree", "HashTagFour", "HashTagFive"],
    case StartIdx =< 20 of
        true ->
            NumUsers = length(UserList),
            RandUid = rand:uniform(NumUsers),
            OperationIndex = rand:uniform(length(Operations)),
            Operation = lists:nth(OperationIndex, Operations),
            io:format("Operation selected: ~p ~n", [Operation]),
            case Operation == "Tweet" of
                true ->
                    io:format("User ~p is tweeting~n", [RandUid]),
                    signIn(ProcessId, RandUid),
                    sendTweet(ProcessId, RandUid, NumUsers);
                false ->
                    case Operation == "Sign_Out" of
                        true ->
                            signOut(ProcessId, RandUid);
                        false ->
                            case Operation == "Sign_In" of
                                true ->
                                    signIn(ProcessId, RandUid);
                                false ->
                                    case Operation == "Subscribe" of
                                        true ->
                                            UidToFollow = rand:uniform(NumUsers),
                                            followUser(NumUsers, UidToFollow, RandUid, ProcessId);
                                        false ->
                                            case Operation == "Get_Hashtags" of
                                                true ->
                                                    SelectHashTag = rand:uniform(5),
                                                    Hash = lists:nth(SelectHashTag, HashTags),
                                                    getHashTagsForUser(Hash, ProcessId);
                                                false ->
                                                    case Operation == "Get_Mentions" of
                                                        true ->
                                                            getMentionsForUser(RandUid, ProcessId);
                                                        false ->
                                                            case Operation == "Get_Feed" of
                                                                true ->
                                                                    getFeedForUser(RandUid, ProcessId);
                                                                false ->
                                                                    case Operation == "Repost" of
                                                                        true ->
                                                                            reTweet(
                                                                                RandUid, ProcessId
                                                                            );
                                                                        false ->
                                                                            ""
                                                                    end
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end,
            simulate(UserList, ProcessId, StartIdx + 1);
        false ->
            ""
    end.

reTweet(Uid, ProcessId) ->
    ProcessId ! {retweet, Uid}.

followUser(NumUsers, FollowUid, CurrUid, ProcessId) ->
    case FollowUid == CurrUid of
        true ->
            NewUserToFollow = rand:uniform(NumUsers),
            followUser(NumUsers, NewUserToFollow, CurrUid, ProcessId);
        false ->
            ProcessId ! {addFollowers, FollowUid, CurrUid}
    end.

getFeedForUser(Uid, ProcessId) ->
    ProcessId ! {get_feed, Uid}.

getMentionsForUser(Uid, ProcessId) ->
    ProcessId ! {display_mentions, Uid}.

getHashTagsForUser(Hash, ProcessId) ->
    ProcessId ! {display_hashtags, Hash}.

sendTweet(ProcessId, Uid, NumUsers) ->
    TweetsWithHashtags = generateRandomTweets(),
    TweetWithMention = constructTweetWithMention(Uid, NumUsers, TweetsWithHashtags),
    io:format("Mention sent by user with UID ~p : ~p ~n", [Uid, [TweetWithMention]]),
    ProcessId ! {postTweet, Uid, TweetWithMention}.

constructTweetWithMention(Uid, NumUsers, TweetWithHashTag) ->
    ChooseMention = rand:uniform(2),
    case ChooseMention == 1 of
        true ->
            User = generateRandomUser(Uid, NumUsers),
            User2 = integer_to_list(User),
            Mention2 = string:concat("@", User2),
            Mention = string:concat(Mention2, " "),
            NewString = string:concat(Mention, TweetWithHashTag),
            NewString;
        false ->
            TweetWithHashTag
    end.

generateRandomUser(CurrUid, NumUsers) ->
    RandUser = rand:uniform(NumUsers),
    case RandUser == CurrUid of
        true ->
            generateRandomUser(CurrUid, NumUsers);
        false ->
            RandUser
    end.

generateRandomTweets() ->
    WordList = ["You", "only", "live", "once", "so", "carpe", "diem"],
    HashTags = ["HashTagOne", "HashTagTwo", "HashTagThree", "HashTagFour", "HashTagFive"],
    GetRandHashTag = rand:uniform(2),
    NumWords = rand:uniform(7),
    List = [rand:uniform(NumWords) || _ <- lists:seq(1, NumWords)],
    Length = length(List),
    GeneratedTweet = genTweet(WordList, "", 1, Length, List),
    case GetRandHashTag == 1 of
        true ->
            SelectHashTag = rand:uniform(5),
            MatchingHashTag = lists:nth(SelectHashTag, HashTags),
            ConcatenatedHash = string:concat("#", MatchingHashTag),
            ConcatString = string:concat(GeneratedTweet, ConcatenatedHash),
            ConcatString;
        false ->
            GeneratedTweet
    end.

genTweet(WordList, String, StartIdx, Length, List) ->
    case StartIdx =< Length of
        true ->
            CurrentIdx = lists:nth(StartIdx, List),
            Str = lists:nth(CurrentIdx, WordList),
            Str2 = string:concat(String, Str),
            Str3 = string:concat(Str2, " "),
            genTweet(WordList, Str3, StartIdx + 1, Length, List);
        false ->
            String
    end.

signOut(ProcessId, Uid) ->
    ProcessId ! {go_offline, Uid}.

signIn(ProcessId, Uid) ->
    ProcessId ! {go_online, Uid}.

checkIfUserIsInList(List, Uid) ->
    Bool = lists:member(Uid, List),
    case Bool of
        true ->
            lists:delete(Uid, List);
        false ->
            List
    end.
