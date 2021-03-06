# aihtml

A simple html render libary more than Mustache Template Complier

## Erlang Mustache Template 

Mustache is a framework-agnostic templating system that enforces separation of view logic from the template
file. Indeed, it is not even possible to embed logic in the template. This
allows templates to be reused across language boundaries and for other
language independent uses.

Working with Mustache means dealing with templates, views, and contexts.
Templates contain HTML (or some other format) and Mustache tags that specify
what data to pull in. A template can be either a string or a file (usually
ending in .mustache). Views are Erlang modules that can define functions that
are called and provide the data for the template tags. A context is an Erlang
dict that contains the current context from which tags can pull data. A few
examples will clarify how these items interact.


## Installation

aihtml uses erlang.mk as its building tool. So currently, it only support erlang.mk. 


## Difference between bbmustache

The target of aihtml is to help user to build a simple view engine in the Erlang. And aihtml uses a modified version muatche compiler from [bbmustache](https://github.com/soranoba/bbmustache). But there are some difference between [bbmustache](https://github.com/soranoba/bbmustache).

bbmustahce:

- It supports the standards mustache sytanx.
- Very light, it won't create any process or ets.
- It can compile mustache file or render directly.
- It can render mustache string directly.

aihtml:

- It also supports the standards mustache sytanx.
- It adds lamda section on mustache sytanx.
- Very heavy, it will create a process and use an ets to store some information.
- It must compile mustache file before rendering, and store the compile result in the ets for resusing.
- It can't render mustache string directly.

## How to use

### Incompatible Changes

In v0.3.5 we start using the atom key to replace the binary key of tags.
So when using v0.3.5 or above, please use atom keys in `context` to render the templates. 

### Bootstrap

aihtml has to bootstrap before rendering mustache files.

It boostraps using function `ai_mustache:bootstrap`, it will using `views` directory as default directory where the mustache files are stored. And will compile all mustache files with the suffix `.mustache` into IR code and store them in ets. 

```erlang
bootstrap()-> ai_mustache_loader:bootstrap().
```

And there is a function which can accept one params settings to change default settings.

```erlang
bootstrap(Settings) -> ai_mustache_loader:bootstrap(Settings).
Settings :: #{ 
    views :=  binary(),
    suffix := binary()
}.
```

###  Render Templates

#### Context

aihtml only support `maps` as context params when it render a mustache file. And the key must be a `binary`.

```erlang
#{
    <<"user">> => #{
        <<"name">> => "David Gao",
        <<"level">> => 1
    },
    <<"stars">> => 10
}
```

#### Partials

aihtml supports partials, and it will auto load the partial mustache file from `views` directory which can be modified by bootstrap.

```erlang
{{> shared/user }}
```

If we use the default settings, aihtml will load `user.mustache` from `views/shared` directory auto.

#### Sections

Section in context is a `list` 
    
friends.mustache
```mustache
<ul> 
  {{# friends }}
    <li>
        <img src="{{{ friends.avatar }}}"/>
        <span>{{ friends.name }}</span>
    </li>
  {{/ friends }}
</ul>
```
friends_context.erl
```erlang
Context = #{
    <<"friends">> => [
        #{<<"name">> => "Jane", <<"avatar">> => "/images/avatar/    jane.png" },
        #{<<"name">> => "David", <<"avatar">> => "/images/avatar/    David.png" },
    ]
}
```


Section in context is `fun/2`
The first param of function will be the rendered binary inside the section.

friends.mustache
```mustache
{{# warpped }}
<ul> 
 {{# friends }}
    <li>
        <img src="{{{ friends.avatar }}}"/>
        <span>{{ friends.name }}</span>
    </li>
 {{/ friends }}
</ul>
{{/ warpped}}
```

friends_context.erl
```erlang
warpped(Acc,Context) -> <<"<div> ",Acc/binary," </div>" >>.
Context = #{
    <<"friends">> => [
        #{<<"name">> => "Jane", <<"avatar">> => "/images/avatar/    jane.png" },
        #{<<"name">> => "David", <<"avatar">> => "/images/avatar/    David.png" },
    ],
    <<"warpped">> => fun warpped/2
}
```

#### Inverted Sections

Inverted section in context is a `list` or not exsist

friends.mustache
```mustache
{{^ friends }}
  <div> Want to know some new friends ? </div>
{{/ friends }}
```

friends_context.erl
```erlang
Context = #{
    <<"friends">> => []
}
```
or 
```erlang
Context = #{}
```

#### Has Section

Has section in context is `map`

navbar.mustache
```mustache
{{+ user }}
    <div>
        <span>{{user.name}}</span>
        <span>{{user.level}}</span>
    </div>
{{/ user }}
```

navbar_context.erl
```erlang
#{
    <<"user">> => #{
        <<"name">> => "David Gao",
        <<"level">> => 1
    }
}
```

Has section in context is `bool` or `binary`

friends.mustache
```mustache
{{+ has_friends }}
<ul> 
 {{# friends }}
    <li>
        <img src="{{{ friends.avatar }}}"/>
        <span>{{ friends.name }}</span>
    </li>
 {{/ friends }}
</ul>
{{/ has_friends}}
```

friends_context.erl
```erlang
Context = #{
    <<"friends">> => [
        #{<<"name">> => "Jane", <<"avatar">> => "/images/avatar/    jane.png" },
        #{<<"name">> => "David", <<"avatar">> => "/images/avatar/    David.png" },
    ],
    <<"has_friends">> => true
}
```

Has section in context is `fun/1`

friends.mustache
```mustache
{{+ has_friends }}
<ul> 
  {{# friends }}
    <li>
      <img src="{{{ friends.avatar }}}"/>
      <span>{{ friends.name }}</span>
    </li>
  {{/ friends }}
</ul>
{{/ has_friends}}
```

friends_context.erl
```erlang
has_friends(Context) ->
    case maps:get(<<"friends>>,Context, undefined) of 
        undefined -> false;
        _ -> true
    end.
Context = #{
    <<"friends">> => [
        #{<<"name">> => "Jane", <<"avatar">> => "/images/avatar/    jane.png" },
        #{<<"name">> => "David", <<"avatar">> => "/images/avatar/    David.png" },
    ],
    <<"has_friends">> => fun has_friends/1
}
```

#### Inverted Has Section

Inverted has section in context is `bool`

friends.mustache
```mustache
{+ has_friends }}
<ul> 
 {{# friends }}
    <li>
        <img src="{{{ friends.avatar }}}"/>
        <span>{{ friends.name }}</span>
    </li>
 {{/ friends }}
</ul>
{{/ has_friends}}
{{- has_friends }}
    <div> Want to know some new friends ? </div>
 {{/ has_friends }}
```

friends_context.erl
```erlang
Context = #{
    <<"friends">> => [
        #{<<"name">> => "Jane", <<"avatar">> => "/images/avatar/    jane.png" },
        #{<<"name">> => "David", <<"avatar">> => "/images/avatar/    David.png" },
    ],
    <<"has_friends">> => true
}
```

Inverted has section in context is `fun/1`

friends.mustache
```mustache
{{+ has_friends }}
<ul> 
 {{# friends }}
    <li>
        <img src="{{{ friends.avatar }}}"/>
        <span>{{ friends.name }}</span>
    </li>
 {{/ friends }}
</ul>
{{/ has_friends}}
{{- has_friends }}
    <div> Want to know some new friends ? </div>
{{/ has_friends }}
```

friends_context.erl
```erlang
has_friends(Context) ->
    case maps:get(<<"friends>>,Context, undefined) of 
        undefined -> false;
        _ -> true
    end.
Context = #{
    <<"friends">> => [
        #{<<"name">> => "Jane", <<"avatar">> => "/images/avatar/    jane.png" },
        #{<<"name">> => "David", <<"avatar">> => "/images/avatar/    David.png" },
    ],
    <<"has_friends">> => fun has_friends/1
}
```

#### lambda

This is an extends of aihtml on mustach syntax.

Lambda in context is `fun/1`
layout.mustache
```mustache
{{* yield}}
```
    
layout_context.erl
```erlang
yield(Context) ->  ......

Context = #{
    <<"yield">> => fun yield/1
}
```

Lambda in context is `fun/2` and a value

layout.mustache
```mustache
{{* yield}}
```
    
layout_context.erl
```erlang
yield(Template,Context)->
    ai_mustache:render(Template,Context).
render(Template,State) -> 
    Context = maps:get(context,State,#{}),
    Layout = maps:get(layout,State,<<"layout/default">>),
    LayoutContext = Context#{ <<"yield">> => [fun yield/2,Template] },
    ai_mustache:render(Layout,LayoutContext).
```

## Projects who use this

- [aiwiki](https://github.com/DavidAlphaFox/aiwiki) a very simple blog.
