{
    "title": "SCP",
    "fields": {
        "code": {
            "label": "Код"
        },
        "title": {
            "label": "Название"
        },
        "foundAt": {
            "label": "Место обнаружения"
        },
        "dangerClass": {
            "type": "select",
            "default": "Евклид",
            "choice": ["Безопасный", "Евклид", "Кетер"],
            "label": "Класс"
        },
        "jobsDone": {
            "label": "Работы завершены",
            "type": "checkbox"
        },
        "conditions": {
            "type": "textarea",
            "label": "Условия содержания"
        },
        "description": {
            "type": "textarea",
            "label": "Описание"
        }
    }
}
