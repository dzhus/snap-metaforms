{
    "title": "SCP",
    "canCreate": ["front"],
    "canRead": ["front", "back", "manager"],
    "canUpdate": ["front", "back"],
    "canDelete": ["manager"],
    "fields": [
        {
            "name": "code",
            "label": "Код",
            "canWrite": true
        },
        {
            "name": "title",
            "label": "Название",
            "canWrite": true
        },
        {
            "name": "foundAt",
            "label": "Место обнаружения",
            "canWrite": true
        },
        {
            "name": "dangerClass",
            "type": "select",
            "default": "Евклид",
            "choice": ["Безопасный", "Евклид", "Кетер"],
            "label": "Класс",
            "canWrite": true
        },
        {
            "name": "jobsDone",
            "label": "Оформление завершено",
            "type": "checkbox",
            "canWrite": true
        },
        {
            "name": "conditions",
            "type": "textarea",
            "label": "Условия содержания",
            "canWrite": true
        },
        {
            "name": "description",
            "type": "textarea",
            "label": "Описание",
            "canWrite": true
        }
    ]
}
