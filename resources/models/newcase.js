{
    "title": "Происшествие",
    "canCreate": ["front"],
    "canRead": ["front", "back", "manager"],
    "canUpdate": ["front", "back"],
    "canDelete": ["back", "manager"],
    "fields": [
        {
            "name":"wazzup", 
            "type":"textarea",
            "label":"Что случилось?",
            "hotkey":"alt+w", 
            "canRead":true,
            "canWrite": ["front"]
        }
        , 
        {
            "name":"vin", 
            "label":"VIN",
            "required":true,
            "details":"car_details", 
            "canRead":true,
            "canWrite":["front"]
        }
        , 
        {
            "name":"program",
            "label":"Программа",
            "data":"programs",
            "canRead":true,
            "canWrite": ["front"]
        }
        , 
        {
            "name":"contact",
            "label":"Клиент",
            "required":true,
            "hotkey":"alt+c",
            "canRead":true,
            "canWrite": ["front"]
        }
        , 
        {
            "name":"contactPhone",
            "label":"Телефон",
            "required":true,
            "canRead":true,
            "canWrite": ["front"]
        }
        , 
        {
            "name":"isProgMember",
            "label":"Участник программы",
            "required":true,
            "type":"checkbox",
            "canRead":true,
            "canWrite": ["front"]
        }
        , 
        {
            "name":"diagnostics1",
            "label":"Диагностика поломки",
            "data":"diagnostics1",
            "type": "textarea",
            "canRead":true,
            "canWrite": ["front"]
        }
        , 
        {
            "name":"services",
            "label":"Оказанные услуги",
            "type": "textarea",
            "canRead":["back", "manager"],
            "canWrite": ["back"]
        }
        , 
        {
            "name":"services",
            "label":"Стоимость услуг",
            "canRead":["back", "manager"],
            "canWrite": ["back"]
        }
    ]
}
