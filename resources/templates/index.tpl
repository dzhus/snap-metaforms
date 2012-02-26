<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Заполнятор</title>
    <link rel="stylesheet" href="/s/css/bootstrap.min.css" />
    <link rel="stylesheet" href="/s/css/local.css" />

    <!-- DOM manipulation -->
    <script src="/s/js/thirdparty/jquery-1.7.1.min.js" />

    <script src="/s/js/thirdparty/knockout-2.0.0.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/s/js/thirdparty/underscore-min.js" />
    <!-- Loose MVC -->
    <script src="/s/js/thirdparty/backbone-0.9.1.min.js" />

    <!-- Knockback is a Knockout + Backbone glue -->
    <script src="/s/js/thirdparty/knockback-0.13.min.js" />

    <!-- Simple templates -->
    <script src="/s/js/thirdparty/mustache.js" />

    <script src="/s/js/metamodel.js" />
    <script src="/s/js/load-model.js" />
  </head>
  <body>
    <div class="container-fluid">
      <div class="row-fluid">
        <div class="span2">
          <div class="box" id="login">
            <ifLoggedIn>
              <div style="float:left;">
                <i class="icon-user" /><loggedInUser /><br />
              </div>
              <div style="float: right;">
                <a href="/logout/">
                  <i class="icon-off" />
                </a>
              </div>
              <div style="clear: both;" />
            </ifLoggedIn>
          </div>

          <div class="box" id="messages" />

          <div class="box" id="timeline" />
        </div>

        <div class="span8">
          <div class="box">
            <fieldset>
              <legend id="model-name" />
              <form id="form" class="form-horizontal" />
            </fieldset>
          </div>
        </div>
      </div>
    </div>

    <!-- Form field templates -->
    <script type="text/template" 
            class="field-template"
            id="textarea-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <textarea class="{{ name }} field span7"
                    name="{{ name }}" 
                    {{# readonly }}disabled{{/ readonly }}
                    rows="7"
                    data-bind="value: {{ name }},
                               valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="text-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <input type="text"
                 class="{{ name }} field span7"
                 name="{{ name }}"
                 {{# readonly }}disabled{{/ readonly }}
                 data-bind="value: {{ name }}, 
                            valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="select-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <select class="{{ name }} field"
                  name="{{ name }}"
                  {{# readonly }}disabled{{/ readonly }}
                  data-bind="value: {{ name }},
                             valueUpdate: 'change'">
            {{# choice }}
            <option value="{{.}}">{{.}}</option>
            {{/ choice }}
          </select>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="checkbox-field-template">
      <div class="control-group">
        <div class="controls">
          <label class="checkbox inline"><input type="checkbox" 
                                                class="{{ name }} field"
                                                name="{{ name }}"
                                                {{# readonly }}disabled{{/ readonly }}
                                                data-bind="checked: {{ name }},
                                                           valueUpdate: 'change'" />
            {{ label }}
          </label>
        </div>
      </div>
    </script>

    <!-- Template for fields with unknown type -->
    <script type="text/template"
            class="field-template"
            id="unknown-field-template">
      <div class="{{ name }} item">
        {{ name }}
      </div>
    </script>
    
    <script type="text/template"
            id="permission-template">
      <div class="form-actions">
        {{# canUpdate }}
        <button class="btn btn-success" type="button"
                onClick="save();">
          <i class="icon-pencil" /> Сохранить</button>
        {{/ canUpdate }}
        {{# canCreate }}
        <button class="btn" type="button"
                onClick="proceed();">
          <i class="icon-file" /> Начать новую</button>
        {{/ canCreate }}
        {{# canDelete }}
        <button class="btn btn-danger" type="button"
                style="float:right;"
                onClick="remove();">
          <i class="icon-trash" /> Удалить</button>
        {{/ canDelete }}
        <div style="clear: both;" />
      </div>
    </script>

    <script type="text/template" id="timeline-item">
      <span class="btn{{# sel }} btn-info{{/ sel }}"
            id="timeline-{{id}}"
            onClick="restore({{ id }});">{{ id }} </span>
    </script>

    <!-- WebSocket notifications -->
    <script type="text/template" id="message-create">
      <li style="color: green;">+{{ id }}</li>
    </script>

    <script type="text/template" id="message-delete">
      <li style="color: red;">-{{ id }}</li>
    </script>
  </body>
</html>
