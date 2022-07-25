tea_include <- function(){
  #IDs added by Anna Reimann, ORCID 0000-0001-8225-7851
  c("56eeaaac-af5e-54d7-9c00-185229da55d5/t1", # Thee-Es. sentz (ocr-mistake, herbal)
    "57a30888-b244-5500-83eb-b7e1d46c39c9/t6", # Thee- (ocr-mistake)
    "f37b9ec6-64d3-5e88-8fcf-6e04914a4a74/t9", # unclear why not included
    "8771cf78-e47a-5639-928d-39972e3ecf5a/t7", # unclear why not included
    "616b230b-1754-5142-8dfb-2b8e468d98ed/t8", # unclear why not included
    "ff449f8f-50e4-5fe3-8542-e32bc0501603/t1", # French (lower case thee)
    "1e08d433-e6a6-5ed6-80fd-d47083706404/t1", # Tea and bookstuff
    "ae058e69-89ff-552e-82ee-05aee3eac66b/t11", # Tea and bookstuff
    "c51f876b-8c7a-5ec9-bcf3-8dca2797a999/t31", # Tea and bookstuff
    "d5c41e50-244d-5e87-99ba-dc2344a3a46f/t13", # Tea and bookstuff
    "e81a8e04-424b-5c48-b1bc-3074c1c3a722/t23", # Tea and bookstuff
    "e1d114b3-fd69-5cf1-9ec5-cace27aadf6d/t21", # Tea and bookstuff
    "f52b9bcc-43c8-5ba9-9360-cd064346334e/t3", # Tea and bookstuff
    "ad240a1b-fb76-5bb5-a22f-d583b39ea235/t35", # Tea and bookstuff
    "e0de3b1f-311c-5dba-b9be-f35dab52dfb3/t1",  # Tea and bookstuff
    "e1d114b3-fd69-5cf1-9ec5-cace27aadf6d/t21", # Tea and bookstuff
    "f52b9bcc-43c8-5ba9-9360-cd064346334e/t3", # Tea and bookstuff
    "ad240a1b-fb76-5bb5-a22f-d583b39ea235/t35", # Tea and bookstuff
    "0b8a3296-8845-5a49-a624-63ad2f8033f8/t10", # Tea and bookstuff
    "550cceb5-074c-54ba-a10d-982bb3ac1471/t14", # Tea and bookstuff
    "2c56f1f6-d183-54e6-b65d-6ed5b02c8cb7/t14", # Tea and bookstuff
    "78bd7f32-06af-536b-b557-840ef3e81df1/t9", # Tea and bookstuff
    "00900c65-9247-54c7-9e1b-1cf8304d0a98/t3", # Tea and bookstuff
    "723f0065-0fa2-5803-927e-4d9866cd3a6b/t12", # Tea and bookstuff
    "bd75d0e4-cd96-549c-98e0-56ee243a43dd/t5", # Tea and bookstuff
    "3a4414f4-e335-5711-a074-816fade6be3d/t5", # Tea and bookstuff
    "4928e655-fc7f-59f0-bac7-391f673b82bd/t14", # Tea and bookstuff
    "2b510de1-fa12-57c4-aa2a-07a60dd87f02/t16", # Tea and bookstuff
    "7b244679-12aa-50d8-82fe-f441b0276e16/t9", # Tea and bookstuff
    "fd834625-25e1-5e1e-94ad-5cbadbc7d95a/t13", # Tea and bookstuff
    "feed9e8e-1b71-5c72-85d7-14f5cc9f552c/t8", # Tea and bookstuff
    "04e9bf31-5a4d-5242-ba51-8ee5bbe3fe54/a7", # Tea and bookstuff
    "d78c476f-e6a6-5972-ae49-ab1308ea62a5/a1", # Tea and bookstuff
    "026eb095-ac68-5f03-a40e-7438ca03c62f/t7", # Tea and bookstuff
    "f4e745da-c838-57e1-bc90-b22b82a314a6/t0", # Tea and bookstuff
    "fa52c30c-4fcd-53e6-9016-879dbfa6fedc/t1", # Tea and bookstuff
    "5149faa2-154d-53d7-ae9b-d5ebe21fb176/t9", # Tea and bookstuff
    "2ab3b5bc-6b82-55d8-a0e8-e4a31ee1efa4/t29", # Tea and bookstuff
    "temp-1814-115-005", # Tea and bookstuff
    "6a1bf8a4-2081-5064-880f-0ad280199c41/a13", # Tea and bookstuff
    "4edfd96f-ea89-52cb-8929-bc3eb1908f30/t2", # unclear why excluded
    "64b616a3-aa58-5072-b7a2-6fa0bc1975b1/t7", # unclear why excluded
    "89a30b6e-d9b5-533b-a2b5-0d619bdb1ac8/t2", # unclear why excluded
    "1d5fa125-effd-5ad2-bf7a-1272b4f88ce2/a8", # unclear why excluded
    "b6fe4597-d0a4-58ff-a23e-701f015eccd2/t11", # unclear why excluded
    "d7d4ef70-2e80-5c77-befa-9958385aee12/t6", # unclear why excluded
    "e887dc42-5e2e-5c1c-a0b9-49bf49a64a69/t4", # unclear why excluded
    "ba56f8d5-2b2a-513d-b230-f9dba4b9f6ec/t8", # unclear why excluded
    "2ae09245-4710-56a2-bfee-0d6cf4d997f9/t4", # unclear why excluded
    "7e8a6e69-bc1a-52b7-840e-92224964614f/t7", # ocr mistake
    "0d9bc887-f9a3-59c4-895d-876332b95591/t4", # ocr mistake
    "badc3509-0129-54a7-9782-57ca2fb0b0ab/t10", # ocr mistake
    "3558e0e1-1d3d-50d9-b4c8-0d84e3b5d339/t1", # ocr mistake
    "d7873185-f697-5d21-b81f-3a5e52b5c2a5/t13", # ocr mistake
    "755c2e0e-e459-5495-bf4e-69df7b6c7b2f/t16", # Tea and bookstuff
    "e8c50c3b-67fa-54e4-85b6-01851760394b/t17", # Tea and bookstuff
    "0dc2b985-a741-511d-b445-9baa6fe83734/t23" # Tea and bookstuff
  )
}