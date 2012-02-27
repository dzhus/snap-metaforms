{
    "title": "Эвакуация",
    "canRead": true,
    "canCreate": ["front"],
    "canUpdate": ["back", "evac"],
    "canDelete": ["manager"],
    "fields":
    [
        {
            "name": "status",
            "label": "Статус услуги",
            "canRead": true,
            "canWrite": ["front", "back", "evac"],
            "type": "select",
            "choice": ["Создание", "Передана в тыл", 
                       "В обработке", "В процессе оказания", 
                       "Оказана", "Оплачена"]
        }
        ,
        {
            "name": "paymentType",
            "label": "Тип оплаты",
            "canRead": ["front", "back"],
            "canWrite": ["front"],
            "type": "select",
            "choice": ["РАМК", "Клиент", 
                       "Смешанный", "Клиент (с возмещением)"]
        }
        ,
        {
            "name": "cost",
            "label": "Стоимость",
            "canRead": ["front", "back", "manager"],
            "canWrite": ["front"]
        }
        ,
        {
            "name": "address",
            "label": "Адрес кейса",
            "canRead": true,
            "canWrite": ["front"]
        }
        ,
        {
            "name": "eta",
            "label": "Ожидаемое время оказания услуги",
            "canRead": true,
            "canWrite": ["front"]
        }
        ,
        {
            "name": "nearestDealer",
            "label": "Ближайший дилерский центр",
            "canRead": ["back", "evac"],
            "canWrite": ["back"]
        }
        ,
        {
            "name": "dealerAgreement",
            "label": "Клиент договорился с ДЦ",
            "type": "checkbox",
            "canRead": ["back", "evac"],
            "canWrite": ["back"]
        }
        ,
        {
            "name": "falseAlarm",
            "label": "Ложный заказ",
            "canWrite": ["back", "evac"],
            "type": "select",
            "choice": ["Нет", "С выставлением счёта", 
                       "Без выставления счёта"]
        }
        ,
        {
            "name": "rta",
            "label": "Реальное время оказания услуги",
            "canRead": ["manager"],
            "canWrite": ["evac"]
        }
    ]
}
