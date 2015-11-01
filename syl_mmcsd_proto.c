/**
 *  \file mmcsd_proto.c
 *
 *  \brief this file defines the MMC/SD standard operations
 *
 */

/*
* Copyright (C) 2010 Texas Instruments Incorporated - http://www.ti.com/
*/
/*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions
*  are met:
*
*    Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
*
*    Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the
*    distribution.
*
*    Neither the name of Texas Instruments Incorporated nor the names of
*    its contributors may be used to endorse or promote products derived
*    from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
*  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
*  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
*  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
*/


#include "syl_mmcsd_proto.h"
#include "string.h"
#include "uartStdio.h"
#include "cache.h"
#include "hw_hs_mmcsd.h"
#include "hw_types.h"
#include "delay.h"
#include "hs_mmcsd.h"


#define DATA_RESPONSE_WIDTH       (4 * SOC_CACHELINE_SIZE)

/* Cache size aligned data buffer (minimum of 64 bytes) for command response */
#ifdef __TMS470__
#pragma DATA_ALIGN(dataBuffer, SOC_CACHELINE_SIZE);
static unsigned char dataBuffer[DATA_RESPONSE_WIDTH];

#elif defined(__IAR_SYSTEMS_ICC__)
#pragma data_alignment = SOC_CACHELINE_SIZE
static unsigned char dataBuffer[DATA_RESPONSE_WIDTH];

#elif defined(gcc)
static unsigned char dataBuffer[DATA_RESPONSE_WIDTH]
                               __attribute__((aligned(SOC_CACHELINE_SIZE)));

#else
#error "Unsupported compiler\n\r"
#endif


extern void delay(unsigned int milliSec);
/**
 * \brief   This function sends the command to MMCSD.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \param    mmcsdCmd It determines the mmcsd cmd
 *
 * \return   status of the command.
 *
 **/
unsigned int MMCSDCmdSend(mmcsdCtrlInfo *ctrl, mmcsdCmd *c)
{
    return ctrl->cmdSend(ctrl, c);
}

/**
 * \brief   This function sends the application command to MMCSD.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \param    mmcsdCmd It determines the mmcsd cmd
 *
 * \return   status of the command.
 *
 **/
unsigned int MMCSDAppCmdSend(mmcsdCtrlInfo *ctrl, mmcsdCmd *c)
{
    unsigned int status = 0;
    mmcsdCmd capp;


    /* APP cmd should be preceeded by a CMD55 */
    capp.idx = SD_CMD(55);
    capp.flags = 0;
    capp.arg = ctrl->card->rca << 16;
    status = MMCSDCmdSend(ctrl, &capp);

    if (status == 0)
    {
        /* return safely, since we cannot continue if CMD55 fails */
    	//инициализация SD карты не прокатила пробуем eMMC CMD1
        return 0;
    }

    status = MMCSDCmdSend(ctrl, c);

    return status;
}

/**
 * \brief   Configure the MMC/SD bus width
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \param   buswidth   SD/MMC bus width.\n
 *
 *  buswidth can take the values.\n
 *     HS_MMCSD_BUS_WIDTH_4BIT.\n
 *     HS_MMCSD_BUS_WIDTH_1BIT.\n
 *
 * \return  None.
 *
 **/
unsigned int MMCSDBusWidthSet(mmcsdCtrlInfo *ctrl)
{
    mmcsdCardInfo *card = ctrl->card;
    unsigned int status = 0;
    mmcsdCmd capp;

    if (card->cardType == MMCSD_CARD_SD)
    {
		capp.idx = SD_CMD(6);
		capp.arg = SD_BUS_WIDTH_1BIT;
		capp.flags = 0;

		if (ctrl->busWidth & SD_BUS_WIDTH_4BIT)
		{
			if (card->busWidth & SD_BUS_WIDTH_4BIT)
			{
				capp.arg = SD_BUS_WIDTH_4BIT;
			}
		}

		capp.arg = capp.arg >> 1;

		status = MMCSDAppCmdSend(ctrl, &capp);

		if (1 == status)
		{
			if (capp.arg == 0)
			{
				ctrl->busWidthConfig(ctrl, SD_BUS_WIDTH_1BIT);
			}
			else
			{
				ctrl->busWidthConfig(ctrl, SD_BUS_WIDTH_4BIT);
			}
		}
		return status;

		}
    else
    { //на проверку шины положили потому что не получается нифига эта проверка)
    	//а не сделать ли нам все по спецификации и не через анус? - не сделать)
		//если надо 4 бита
		if (card->busWidth & SD_BUS_WIDTH_4BIT)
		{
			if (ctrl->busWidth & SD_BUS_WIDTH_4BIT)
			{
				//тест шины 4 бита - забили на него
				ctrl->busWidthConfig(ctrl, SD_BUS_WIDTH_4BIT);
				capp.idx = SD_CMD(6);
				capp.flags = SD_CMDRSP_BUSY;
				capp.arg = 0x03B70100;
				status = ctrl->cmdSend(ctrl, &capp);
				if (status == 0)  return 0;

			}
		}
		else if (card->busWidth & SD_BUS_WIDTH_8BIT) //хотим 8 бит
		{
			if (ctrl->busWidth & SD_BUS_WIDTH_8BIT)
			{
				//тест шины 8 бит - забили на него
				ctrl->busWidthConfig(ctrl, SD_BUS_WIDTH_8BIT);
				capp.idx = SD_CMD(6);
				capp.flags = SD_CMDRSP_BUSY;
				capp.arg = 0x03B70200;
				status = ctrl->cmdSend(ctrl, &capp);
				if (status == 0)  return 0;
				//аццки важный фикс!! без этого вылетает!
				//ждем пока карта не снимет busy!
		        while (!(HWREG(ctrl->memBase + MMCHS_PSTATE) & (unsigned int)BIT(20)));

			}
		}



    }
    return 1; //какие мы молодцы!
}

/**
 * \brief    This function configures the transmission speed in MMCSD.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  1 - successfull.
 *           0 - failed.
 **/
unsigned int MMCSDTranSpeedSet(mmcsdCtrlInfo *ctrl)
{
    mmcsdCardInfo *card = ctrl->card;
    unsigned int speed;
    int status;
    unsigned int cmdStatus = 0;
    mmcsdCmd cmd;


    if (card->cardType == MMCSD_CARD_SD)
    {
    	ctrl->xferSetup(ctrl, 1, dataBuffer, 64, 1);
        cmd.idx = SD_CMD(6);
        cmd.arg = ((SD_SWITCH_MODE & SD_CMD6_GRP1_SEL) | (SD_CMD6_GRP1_HS));
        cmd.flags = SD_CMDRSP_READ | SD_CMDRSP_DATA | (ctrl->dmaEnable << SD_CMDRSP_DMAEN_OFFSET);
        cmd.nblks = 1;
        cmd.data = (signed char*)dataBuffer;

        cmdStatus = ctrl->cmdSend(ctrl, &cmd);

        if (cmdStatus == 0)
        {
            return 0;
        }

        cmdStatus = ctrl->xferStatusGet(ctrl);

        if (cmdStatus == 0)
        {
            return 0;
        }

        /* Invalidate the data cache. */
        CacheDataInvalidateBuff((unsigned int) dataBuffer, DATA_RESPONSE_WIDTH);

        speed = card->tranSpeed;

        if ((dataBuffer[16] & 0xF) == SD_CMD6_GRP1_HS)
        {
            card->tranSpeed = SD_TRANSPEED_50MBPS;
        }

        if (speed == SD_TRANSPEED_50MBPS)
        {
            status = ctrl->busFreqConfig(ctrl, 50000000);
            ctrl->opClk = 50000000;
        }
        else
        {
            status = ctrl->busFreqConfig(ctrl, 25000000);
            ctrl->opClk = 25000000;
        }

        if (status != 0)
        {
            return 0;
        }

    }
    else
    {
    	//при инициализации eMMC уже была выставлена максимальная частота
    	//на которую может быть настроена карта без HS_TIMING по TRAN_SPEED из CSD
    	//теперь если highspeed = 1 делаем 48 МГц
    	if (card->sd_ver <= 1) return 1; //карта уже на макс скорости
    	cmd.idx = SD_CMD(6);
    	cmd.flags = SD_CMDRSP_BUSY;
    	cmd.arg = 0x03B90100;
		status = ctrl->cmdSend(ctrl, &cmd);
		if (status == 0)  return 0;

        status = ctrl->busFreqConfig(ctrl, 48000000);
        if (status != 0) return 0;
        ctrl->opClk = 48000000;
        //тут проблемка, не сразу после выхода из этой функции карта готова
        //нужна задержка какая-то
        //наерно если подождать пока DAT0 не поднимется в '1' будет норм
        while (!(HWREG(ctrl->memBase + MMCHS_PSTATE) & (unsigned int)BIT(20)));

    }

    return 1;
}

/**
 * \brief   This function resets the MMCSD card.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  1 - successfull reset of card.
 *           0 - fails to reset the card.
 **/
unsigned int MMCSDCardReset(mmcsdCtrlInfo *ctrl)
{
    unsigned int status = 0;
    mmcsdCmd cmd;

    cmd.idx = SD_CMD(0);

    cmd.flags = SD_CMDRSP_NONE;
    cmd.arg = 0;

    status = ctrl->cmdSend(ctrl, &cmd);

    return status;
}

/**
 * \brief   This function sends the stop command to MMCSD card.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  1 - successfully sends stop command to card.
 *           0 - fails to send stop command to card.
 **/
unsigned int MMCSDStopCmdSend(mmcsdCtrlInfo *ctrl)
{
    unsigned int status = 0;
    mmcsdCmd cmd;

    cmd.idx  = SD_CMD(12);
    cmd.flags = SD_CMDRSP_BUSY;
    cmd.arg = 0;

    ctrl->cmdSend(ctrl, &cmd);

    /* Get transfer status */
    status = ctrl->xferStatusGet(ctrl);

    return status;
}

/**
 * \brief   This function determines the type of MMCSD card.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  type of the MMCSD card
 *
 **/
unsigned int MMCSDCardTypeCheck(mmcsdCtrlInfo * ctrl)
{
    unsigned int status;
    mmcsdCmd cmd;

    /*
     * Card type can be found by sending CMD55. If the card responds,
     * it is a SD card. Else, we assume it is a MMC Card
     */

    cmd.idx = SD_CMD(55);
    cmd.flags = 0;
    cmd.arg = 0;
    status = MMCSDAppCmdSend(ctrl, &cmd);

    return status;
}

/**
 * \brief   This function intializes the mmcsdcontroller.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  1 - Intialization is successfull.
 *           0 - Intialization is failed.
 **/
unsigned int MMCSDCtrlInit(mmcsdCtrlInfo *ctrl)
{
    return ctrl->ctrlInit(ctrl);
}

/**
 * \brief   This function determines whether card is persent or not.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  1 - Card is present.
 *           0 - Card is not present.
 **/
unsigned int MMCSDCardPresent(mmcsdCtrlInfo *ctrl)
{
    return ctrl->cardPresent(ctrl);
}

/**
 * \brief   Enables the controller events to generate a h/w interrupt request
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \return   none
 *
 **/
void MMCSDIntEnable(mmcsdCtrlInfo *ctrl)
{
    ctrl->intrEnable(ctrl);

    return;
}

/**
 * \brief   This function intializes the MMCSD Card.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 *
 * \returns  1 - Intialization is successfull.
 *           0 - Intialization is failed.
 **/
unsigned int MMCSDCardInit(mmcsdCtrlInfo *ctrl)
{

    mmcsdCardInfo *card = ctrl->card;
    unsigned int retry = 0xFFFF;
    unsigned int status = 0;
    unsigned int khz;
    mmcsdCmd cmd;

    memset(ctrl->card, 0, sizeof(mmcsdCardInfo));

    card->ctrl = ctrl;

    /* CMD0 - reset card */
    status = MMCSDCardReset(ctrl);

    if (status == 0)
    {
        return 0;
    }

    /* Returns 1 for a SD card, 0 for a non-SD card */
    status = MMCSDCardTypeCheck(ctrl);

    if (status == 1)
    /* SD Card */
    {
    	#if 0
        ctrl->card->cardType = MMCSD_CARD_SD;

        /* CMD0 - reset card */
        status = MMCSDCardReset(ctrl);

        if (status == 0)
        {
            return 0;
        }

        /* CMD8 - send oper voltage */
        cmd.idx = SD_CMD(8);
        cmd.flags = 0;
        cmd.arg = (SD_CHECK_PATTERN | SD_VOLT_2P7_3P6);

        status = MMCSDCmdSend(ctrl, &cmd);

        if (status == 0)
        {
            /* If the cmd fails, it can be due to version < 2.0, since
             * we are currently supporting high voltage cards only
             */
        }

        /* Go ahead and send ACMD41, with host capabilities */
        cmd.idx = SD_CMD(41);
        cmd.flags = 0;
        cmd.arg = SD_OCR_HIGH_CAPACITY | SD_OCR_VDD_WILDCARD;

        status = MMCSDAppCmdSend(ctrl,&cmd);

        if (status == 0)
        {
            return 0;
        }

        /* Poll until we get the card status (BIT31 of OCR) is powered up */
        do {
                cmd.idx = SD_CMD(41);
                cmd.flags = 0;
                cmd.arg = SD_OCR_HIGH_CAPACITY | SD_OCR_VDD_WILDCARD;

                MMCSDAppCmdSend(ctrl,&cmd);

        } while (!(cmd.rsp[0] & ((unsigned int)BIT(31))) && retry--);

        if (retry == 0)
        {
            /* No point in continuing */
            return 0;
        }

        card->ocr = cmd.rsp[0];

        card->highCap = (card->ocr & SD_OCR_HIGH_CAPACITY) ? 1 : 0;

        /* Send CMD2, to get the card identification register */
        cmd.idx = SD_CMD(2);
        cmd.flags = SD_CMDRSP_136BITS;
        cmd.arg = 0;

        status = MMCSDCmdSend(ctrl,&cmd);

        memcpy(card->raw_cid, cmd.rsp, 16);

        if (status == 0)
        {
            return 0;
        }

        /* Send CMD3, to get the card relative address */
        cmd.idx = SD_CMD(3);
        cmd.flags = 0;
        cmd.arg = 0;

        status = MMCSDCmdSend(ctrl,&cmd);

        card->rca = SD_RCA_ADDR(cmd.rsp[0]);

        if (status == 0)
        {
            return 0;
        }

        /* Send CMD9, to get the card specific data */
        cmd.idx = SD_CMD(9);
        cmd.flags = SD_CMDRSP_136BITS;
        cmd.arg = card->rca << 16;

        status = MMCSDCmdSend(ctrl,&cmd);

        memcpy(card->raw_csd, cmd.rsp, 16);

        if (status == 0)
        {
            return 0;
        }

        if (SD_CARD_CSD_VERSION(card))
        {
            card->tranSpeed = SD_CARD1_TRANSPEED(card);
            card->blkLen = 1 << (SD_CARD1_RDBLKLEN(card));
            card->size = SD_CARD1_SIZE(card);
            card->nBlks = card->size / card->blkLen;
        }
        else
        {
            card->tranSpeed = SD_CARD0_TRANSPEED(card);
            card->blkLen = 1 << (SD_CARD0_RDBLKLEN(card));
            card->nBlks = SD_CARD0_NUMBLK(card);
            card->size = SD_CARD0_SIZE(card);

        }

        /* Set data block length to 512 (for byte addressing cards) */
        if( !(card->highCap) )
        {
            cmd.idx = SD_CMD(16);
            cmd.flags = SD_CMDRSP_NONE;
            cmd.arg = 512;
            status = MMCSDCmdSend(ctrl,&cmd);

            if (status == 0)
            {
                return 0;
            }
            else
            {
                card->blkLen = 512;
            }
        }

        /* Select the card */
        cmd.idx = SD_CMD(7);
        cmd.flags = SD_CMDRSP_BUSY;
        cmd.arg = card->rca << 16;

        status = MMCSDCmdSend(ctrl,&cmd);

        if (status == 0)
        {
            return 0;
        }

        /*
         * Send ACMD51, to get the SD Configuration register details.
         * Note, this needs data transfer (on data lines).
         */
        cmd.idx = SD_CMD(55);
        cmd.flags = 0;
        cmd.arg = card->rca << 16;

        status = MMCSDCmdSend(ctrl,&cmd);
        if (status == 0)
        {
            return 0;
        }

        ctrl->xferSetup(ctrl, 1, dataBuffer, 8, 1);

        cmd.idx = SD_CMD(51);
        cmd.flags = SD_CMDRSP_READ | SD_CMDRSP_DATA | (ctrl->dmaEnable << SD_CMDRSP_DMAEN_OFFSET);
        cmd.arg = card->rca << 16;
        cmd.nblks = 1;
        cmd.data = (signed char*)dataBuffer;

        status = ctrl->cmdSend(ctrl, &cmd);
        if (status == 0)
        {
            return 0;
        }

        status = ctrl->xferStatusGet(ctrl);

        if (status == 0)
        {
            return 0;
        }

        /* Invalidate the data cache. */
        CacheDataInvalidateBuff((unsigned int)dataBuffer, DATA_RESPONSE_WIDTH);

        card->raw_scr[0] = (dataBuffer[3] << 24) | (dataBuffer[2] << 16) | \
		                   (dataBuffer[1] << 8) | (dataBuffer[0]);
        card->raw_scr[1] = (dataBuffer[7] << 24) | (dataBuffer[6] << 16) | \
                                   (dataBuffer[5] << 8) | (dataBuffer[4]);

        card->sd_ver = SD_CARD_VERSION(card);
        card->busWidth = SD_CARD_BUSWIDTH(card);
#endif
    }

    else

    {
#if 1
    	//mmc card initialization


        ctrl->card->cardType = MMCSD_CARD_MMC;
        //open drain для команд инициализации
        HWREG(ctrl->memBase + MMCHS_CON) |= MMCHS_CON_OD;

        //Set SD_SYSCTL[25] SRC
        //bit to 0x1 and wait until it returns to 0x0
        HWREG(ctrl->memBase + MMCHS_SYSCTL) |= MMCHS_SYSCTL_SRC;
        while(!(HWREG(ctrl->memBase + MMCHS_SYSCTL) & MMCHS_SYSCTL_SRC));
        while(HWREG(ctrl->memBase + MMCHS_SYSCTL) & MMCHS_SYSCTL_SRC);

       /* CMD1 - SEND_OP_COND */
        retry = 10; //с потолка
        cmd.idx = SD_CMD(1);
        cmd.flags = 0;
        cmd.arg = 0x00ff8080;/////карта до 2 Гб?
        cmd.rsp[0] = 0;
	do{
		status = ctrl->cmdSend(ctrl, &cmd);
		if (status == 0) return status; //если нет ответа, можно выходить

	} while (!(cmd.rsp[0] & ((unsigned int)BIT(31))) && retry--);

		if (0xffffffff == retry) //карта больше 2 Гб?
		{
			retry = 10; //c потолка
	        cmd.arg = 0x40ff8080; //волшебная цыфорка
			do{
				status = ctrl->cmdSend(ctrl, &cmd);
				if (status == 0) return status; //если нет ответа, можно выходить

			} while (!(cmd.rsp[0] & ((unsigned int)BIT(31))) && retry--);

		}
		if (0xffffffff == retry) return 0;

		//сохраняем OCR
        card->ocr = cmd.rsp[0];
        card->highCap = (card->ocr & SD_OCR_HIGH_CAPACITY) ? 1 : 0;

       /* CMD2 - ALL_SEND_CID */
        cmd.idx = SD_CMD(2);
        cmd.flags = SD_CMDRSP_136BITS;
        cmd.arg = 0;
		status = ctrl->cmdSend(ctrl, &cmd);
		if (status == 0) return status;

		//Сохраняем CID карты
        memcpy(card->raw_cid, cmd.rsp, 16);

      /* CMD3 - SET_RELATIVE_ADDR */
        cmd.idx = SD_CMD(3);
        cmd.flags = 0;
        cmd.arg = 2 << 16;
		status = ctrl->cmdSend(ctrl, &cmd);
		if (status == 0) return status;

        card->rca = 2; //тупо

        //вырубаем open drain для команд инициализации
        HWREG(ctrl->memBase + MMCHS_CON) &= ~MMCHS_CON_OD;


        /* Send CMD9, to get the card specific data */
         cmd.idx = SD_CMD(9);
         cmd.flags = SD_CMDRSP_136BITS;
         cmd.arg = card->rca << 16;

 		status = ctrl->cmdSend(ctrl, &cmd);
		if (status == 0) return status;

         memcpy(card->raw_csd, cmd.rsp, 16);

         card->sd_ver =  SD_CARD_CSD_VERSION(card);
         card->tranSpeed = SD_CARD0_TRANSPEED(card);

         //Меняем тактовую частоту на повыше
         //если где-то еще используется - запихнуть в функцию
         switch (card->tranSpeed & 0x00000007) {
         case 0:
        	 khz = 100e3;
        	 break;
         case 1:
          	 khz = 1000e3;
           	 break;
         case 2:
           	 khz = 10000e3;
          	 break;
         case 3:
           	 khz = 100000e3;
          	 break;
         default:
             UARTPuts("TRAN_SPEED incorrect value read", -1);
        	 return 0;
         }
         switch ((card->tranSpeed) >> 3) {
         case 1:
        	 ctrl->opClk = 1 * khz;
        	 break;
         case 2:
        	 ctrl->opClk = 1.2 * khz;
        	 break;
         case 3:
        	 ctrl->opClk = 1.3 * khz;
        	 break;
         case 4:
        	 ctrl->opClk = 1.5 * khz;
        	 break;
         case 5:
        	 ctrl->opClk = 2 * khz;
        	 break;
         case 6:
        	 ctrl->opClk = 2.6 * khz;
        	 break;
         case 7:
        	 ctrl->opClk = 3 * khz;
        	 break;
         case 8:
        	 ctrl->opClk = 3.5 * khz;
        	 break;
         case 9:
        	 ctrl->opClk = 4 * khz;
        	 break;
         case 10:
        	 ctrl->opClk = 4.5 * khz;
        	 break;
         case 11:
        	 ctrl->opClk = 5.2 * khz;
        	 break;
         case 12:
        	 ctrl->opClk = 5.5 * khz;
        	 break;
         case 13:
        	 ctrl->opClk = 6 * khz;
        	 break;
         case 14:
        	 ctrl->opClk = 7 * khz;
        	 break;
         case 15:
        	 ctrl->opClk = 8 * khz;
        	 break;
         default:
             UARTPuts("TRAN_SPEED incorrect value read", -1);
        	 return 0;
         }
         status = ctrl->busFreqConfig(ctrl, ctrl->opClk);

         if (status != 0) //эта функция возвращает ноль при успехе
         {
             UARTPuts("HS MMC/SD TRAN_SPEED freqval set failed\n\r", -1);
         }
        //если спецификация вер. 4.0 и выше
         if (card->sd_ver > 1)
		 {
             /* Send CMD7 select card */
              cmd.idx = SD_CMD(7);
              cmd.flags = 0; //ответ R1
              cmd.arg = card->rca << 16;

      		status = ctrl->cmdSend(ctrl, &cmd);
     		if (status == 0) return status;

        	 //надо затянуть EXT_CSD
			  ctrl->xferSetup(ctrl, 1, dataBuffer, 512, 1);
			  cmd.idx = SD_CMD(8);
			  cmd.flags = SD_CMDRSP_READ | SD_CMDRSP_DATA | (ctrl->dmaEnable << SD_CMDRSP_DMAEN_OFFSET);
			  cmd.arg = 0;
			  cmd.nblks = 1;
			  cmd.data = (signed char*)dataBuffer;
			  status = ctrl->cmdSend(ctrl, &cmd);
			  if (status == 0)  return 0;

			  status = ctrl->xferStatusGet(ctrl);
			  if (status == 0) return 0;

			  /* Invalidate the data cache. */
			  CacheDataInvalidateBuff((unsigned int)dataBuffer, DATA_RESPONSE_WIDTH);

		}
         else
         {
        	 UARTPuts("Unsupported eMMC\n\r", -1);
        	 return 0;
         }

         //разное определение размера карты для highCap карт  и обычных карт
         if (!(card->highCap))
         { //не факт, что работает для !highCap
			 card->blkLen = 1 << (SD_CARD0_RDBLKLEN(card));
			 card->size = SD_CARD0_SIZE(card);
			 card->nBlks = card->size / card->blkLen;

        /* Set data block length to 512 (for byte addressing cards) */
			 if (card->blkLen != 512)
			 {
				 cmd.idx = SD_CMD(16);
				 cmd.flags = 0; //resp R1
				 cmd.arg = 512;
				 status = MMCSDCmdSend(ctrl,&cmd);

				 if (status == 0)
				 {
					 return 0;
				 }
				 else
				 {
					 card->blkLen = 512;
				 }

			 }
         }
         else //highcap - берем sector size из EXT_CSD
         {
			  //надо сохранить нужную инфу из EXT_CSD на будущее
			  //пока только SEC_COUNT
			 card->blkLen = 512;
			 card->nBlks = (dataBuffer[212] << 0) |
					  (dataBuffer[213] << 8) |
					  (dataBuffer[214] << 16) |
					  (dataBuffer[215] << 24);
			 card->size = card->nBlks; //для highcap карты размер в секторах
         }
         card->busWidth = 1;
#if 0 //деселект рабочий но забили на него - карта у нас одна
         //deselect card
         /* Send CMD7 select card */
        cmd.idx = SD_CMD(7);
        cmd.flags = SD_CMDRSP_NONE; //ответ R1
        cmd.arg = 0; //rca = 0
    	status = ctrl->cmdSend(ctrl, &cmd);
   		if (status == 0) return status;
#endif
		//end emmc initialization
       #endif
    }

    return 1;
}

/**
 * \brief   This function sends the write command to MMCSD card.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 * \param    ptr           It determines the address from where data has to written
 * \param    block         It determines to which block data to be written
 * \param    nblks         It determines the number of blocks to be written
 *
 * \returns  1 - successfull written of data.
 *           0 - failure to write the data.
 **/
unsigned int MMCSDWriteCmdSend(mmcsdCtrlInfo *ctrl, void *ptr, unsigned int block,
                               unsigned int nblks)
{
    mmcsdCardInfo *card = ctrl->card;
    unsigned int status = 0;
    unsigned int address;
    mmcsdCmd cmd;

    /*
     * Address is in blks for high cap cards and in actual bytes
     * for standard capacity cards
     */
    if (!MMCSDWaitCardReadyForData(ctrl)) return 0;

    if (card->highCap)
    {
        address = block;
    }
    else
    {
        address = block * card->blkLen;
    }

    /* Clean the data cache. */
    CacheDataCleanBuff((unsigned int) ptr, (512 * nblks));

    ctrl->xferSetup(ctrl, 0, ptr, 512, nblks);

    cmd.flags = SD_CMDRSP_WRITE | SD_CMDRSP_DATA | (ctrl->dmaEnable << SD_CMDRSP_DMAEN_OFFSET);
    cmd.arg = address;
    cmd.nblks = nblks;

    if (nblks > 1)
    {
        cmd.idx = SD_CMD(25);
//        cmd.flags |= SD_CMDRSP_ABORT;
    }
    else
    {
        cmd.idx = SD_CMD(24);
    }


    status = ctrl->cmdSend(ctrl, &cmd);

    if (status == 0)
    {
        return 0;
    }

    status = ctrl->xferStatusGet(ctrl);

    if (status == 0)
    {
        return 0;
    }

    /* Send a STOP *//*
    if (cmd.nblks > 1)
    {
        status = MMCSDStopCmdSend(ctrl);

        if (status == 0)
        {
            return 0;
        }
    }*/

    return 1;
}

/**
 * \brief   This function sends the write command to MMCSD card.
 *
 * \param    mmcsdCtrlInfo It holds the mmcsd control information.
 * \param    ptr           It determines the address to where data has to read
 * \param    block         It determines from which block data to be read
 * \param    nblks         It determines the number of blocks to be read
 *
 * \returns  1 - successfull reading of data.
 *           0 - failure to the data.
 **/
unsigned int MMCSDReadCmdSend(mmcsdCtrlInfo *ctrl, void *ptr, unsigned int block,
                              unsigned int nblks)
{
    mmcsdCardInfo *card = ctrl->card;
    unsigned int status = 0;
    unsigned int address;
    mmcsdCmd cmd;

    /*
     * Address is in blks for high cap cards and in actual bytes
     * for standard capacity cards
     */
    if (card->highCap)
    {
        address = block;
    }
    else
    {
        address = block * card->blkLen;
    }

    ctrl->xferSetup(ctrl, 1, ptr, 512, nblks);

    cmd.flags = SD_CMDRSP_READ | SD_CMDRSP_DATA | (ctrl->dmaEnable << SD_CMDRSP_DMAEN_OFFSET);
    cmd.arg = address;
    cmd.nblks = nblks;

    if (nblks > 1)
    {
//        cmd.flags |= SD_CMDRSP_ABORT; //фтопку этот шлак, в функции посылки прописан флаг ACMD12
        cmd.idx = SD_CMD(18);
    }
    else
    {
        cmd.idx = SD_CMD(17);
    }

    status = ctrl->cmdSend(ctrl, &cmd);
    if (status == 0)
    {
        return 0;
    }

    status = ctrl->xferStatusGet(ctrl);

    if (status == 0)
    {
        return 0;
    }

    /* Send a STOP */
    /*фтопку этот шлак, в функции посылки прописан флаг ACMD12
    if (cmd.nblks > 1)
    {
        status = MMCSDStopCmdSend(ctrl);

        if (status == 0)
        {
            return 0;
        }
    }
*/
    /* Invalidate the data cache. */
    CacheDataInvalidateBuff((unsigned int) ptr, (512 * nblks));

    return 1;
}

//Будем ждать готовности карты в течение времени таймаута и возвращать 1 в случае успеха
//Функция добавлена КП Дорошенко для фикса глюка карты, когда после инициализации или записи
//карта не сразу готова к новым подвигам
unsigned int MMCSDWaitCardReadyForData(mmcsdCtrlInfo *ctrl)
{
    mmcsdCardInfo *card = ctrl->card;
    unsigned int status = 0;
    mmcsdCmd cmd;
    unsigned int retry = 0xffffffff;

    cmd.idx = SD_CMD(13);
    cmd.flags = 0;
    cmd.arg = card->rca << 16;

	do{
		status = ctrl->cmdSend(ctrl, &cmd);
		if (status == 0) return status; //если нет ответа, можно выходить

	} while (!(cmd.rsp[0] & ((unsigned int)BIT(8))) && --retry); //кол-во попыток вышло, либо стоит бит READY_FOR_DATA

	if (0 == retry) return 0;

	return 1;
}
