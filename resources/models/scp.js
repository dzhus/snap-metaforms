{
    "title": "SCP",
    "canCreate": ["front"],
    "canRead": true,
    "canUpdate": ["back", "manager"],
    "canDelete": ["front", "manager"],
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
            "canRead": ["front"]
        },
        {
            "name": "dangerClass",
            "type": "select",
            "default": "Евклид",
            "choice": ["Безопасный", "Евклид", "Кетер"],
            "label": "Класс",
            "canWrite": ["front"]
        },
        {
            "name": "jobsDone",
            "label": "Оформление завершено",
            "type": "checkbox",
            "canWrite": ["front"]
        },
        {
            "name": "conditions",
            "type": "textarea",
            "label": "Условия содержания",
            "canWrite": ["back"]
        },
        {
            "name": "description",
            "type": "textarea",
            "label": "Описание",
            "canWrite": ["back"]
        }
    ]
}
